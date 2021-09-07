//! Ruth's thtandard library.

mod collections;
mod control;
mod env;
mod eq;
mod exceptions;
mod fmt;
mod funcs;
mod math;
mod misc;
mod pairs_lists;
mod quoting;
mod strings;
mod symbols;
use collections::*;
use control::*;
use env::*;
use eq::*;
use exceptions::*;
use fmt::*;
use funcs::*;
use math::*;
use misc::*;
use pairs_lists::*;
use quoting::*;
use strings::*;
use symbols::*;

use std::{fs, io::Write, path::Path};

use gc::{Gc, GcCell};

use crate::{Engine, EvalResult, Exception, Expr, Namespace};

const THTD_LIB_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/thtdlib");

pub fn add_thtandard_library(engine: &mut Engine) {
    let thtdlib = engine.thtdlib();

    for (name, special_form) in [
        ("quote", quote as _),
        ("quasiquote", quasiquote as _),
        ("unquote", unquote as _),
        ("unquote-splicing", unquote_splicing as _),
        ("define", define as _),
        ("define-macro", define_macro as _),
        ("lambda", lambda as _),
        ("macro", macro_ as _),
        ("if", if_ as _),
        ("do", do_ as _),
        ("let", let_ as _),
        ("and", and as _),
        ("or", or as _),
        ("lazy-cons", lazy_cons as _),
        ("catch", catch as _),
        ("with-handler", with_handler as _),
    ] {
        let symbol = engine.intern_symbol(name);
        let handle = Gc::new(Expr::SpecialForm {
            func: special_form,
            name: symbol,
        });
        thtdlib.borrow_mut().insert(symbol, handle);
    }

    for (name, native_func) in [
        // quoting
        ("eval", eval as _),
        // math
        ("+", add as _),
        ("-", sub as _),
        ("*", mul as _),
        ("/", div as _),
        ("%", rem as _),
        ("**", pow as _),
        ("<", lt as _),
        (">", gt as _),
        ("<=", le as _),
        (">=", ge as _),
        ("=", num_eq as _),
        ("not", not as _),
        ("xor", xor as _),
        ("round", round as _),
        ("trunc", trunc as _),
        ("floor", floor as _),
        ("ceil", ceil as _),
        ("->inexact", to_inexact as _),
        ("bitand", bitwise_and as _),
        ("bitor", bitwise_or as _),
        ("bitxor", bitwise_xor as _),
        ("bitnot", bitwise_not as _),
        ("bitshift", bitwise_shift as _),
        ("bitrot", bitwise_rotate as _),
        ("bitcount", popcnt as _),
        // string
        ("string", to_string as _),
        ("string-len", string_len as _),
        ("prn", prn as _),
        ("string-slice", string_slice as _),
        ("string-find", string_find as _),
        ("string-replace", string_replace as _),
        ("string-lines", string_lines as _),
        // formatting
        ("scanf", scanf as _),
        ("read", read as _),
        ("write", write as _),
        // list/pair
        ("cons", cons as _),
        ("car", car as _),
        ("cdr", cdr as _),
        // symbols
        ("string->symbol", string2symbol as _),
        ("symbol->string", symbol2string as _),
        // exceptions
        ("exception", make_exception as _),
        // equality
        ("ptr-equal?", id_equal as _),
        ("equal?", equal as _),
        ("pair?", is_pair as _),
        ("number?", is_number as _),
        ("exact?", is_exact as _),
        ("inexact?", is_inexact as _),
        ("nil?", is_nil as _),
        ("string?", is_string as _),
        ("symbol?", is_symbol as _),
        ("callable?", is_callable as _),
        ("procedure?", is_procedure as _),
        ("macro?", is_macro as _),
        // collections
        ("new-map", new_map as _),
        ("map-insert", map_insert as _),
        ("map-get", map_get as _),
        ("map-remove", map_remove as _),
        ("map->list", map2list as _),
        // etc
        ("reload-thtdlib", reload_thtd as _),
        ("timeit", timeit as _),
    ] {
        let symbol = engine.intern_symbol(name);
        let handle = Gc::new(Expr::NativeProcedure {
            func: Ok(native_func),
            name: symbol,
        });
        thtdlib.borrow_mut().insert(symbol, handle);
    }
    for (name, tail_func) in [("apply", apply as _)] {
        let symbol = engine.intern_symbol(name);
        let handle = Gc::new(Expr::NativeProcedure {
            func: Err(tail_func),
            name: symbol,
        });
        thtdlib.borrow_mut().insert(symbol, handle);
    }

    // and the thtdlib impled in ruth itself
    load_thtd_lib(engine).unwrap();
}

fn load_thtd_lib(engine: &mut Engine) -> EvalResult {
    let nil_out = Ok(Gc::new(Expr::Nil));
    let thtd_path = Path::new(THTD_LIB_ROOT);

    let mut paths = Vec::new();

    let mut todo = vec![thtd_path.to_path_buf()];
    while let Some(path) = todo.pop() {
        if path.is_dir() {
            for entry in fs::read_dir(path).unwrap() {
                let entry = entry.unwrap();
                let path = entry.path();
                todo.push(path)
            }
        } else {
            paths.push(path);
        }
    }

    paths.sort_unstable();

    for path in paths {
        // Run the callback
        let name = path.to_string_lossy().into_owned();
        let source = fs::read_to_string(&name).unwrap();
        let res = engine.read_eval(&source, name.to_owned());
        match res {
            Ok(Err(ono)) => {
                let ono = ono.into_expr(engine);
                println!("{}", engine.write_expr(ono).unwrap());
                return nil_out;
            }
            Ok(Ok(_)) => {}
            Err(e) => {
                e.report()
                    .eprint(ariadne::sources(std::iter::once((name, source))))
                    .unwrap();
                return nil_out;
            }
        }
    }

    nil_out
}

// Function to reload
fn reload_thtd(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 0, 0)?;
    load_thtd_lib(engine)
}

// "Contract" functions

pub fn check_argc(
    engine: &mut Engine,
    args: &[Gc<Expr>],
    min: usize,
    max: usize,
) -> Result<(), Exception> {
    if !(min..=max).contains(&args.len()) {
        let msg = if min == max {
            format!("expected exactly {} args but got {}", min, args.len())
        } else {
            format!(
                "expected between {} and {} args but got {}",
                min,
                max,
                args.len()
            )
        };
        let data = Engine::list_to_sexp(&[
            Gc::new(Expr::Integer(min as _)),
            Gc::new(Expr::Integer(max as _)),
            Gc::new(Expr::Integer(args.len() as _)),
            Engine::list_to_sexp(args),
        ]);
        Err(engine.make_err("application/argc", msg, Some(data)))
    } else {
        Ok(())
    }
}

pub fn check_min_argc(engine: &mut Engine, args: &[Gc<Expr>], min: usize) -> Result<(), Exception> {
    if min > args.len() {
        let msg = format!("expected {} args or more but got {}", min, args.len());
        let data = Engine::list_to_sexp(&[
            Gc::new(Expr::Integer(min as _)),
            Gc::new(Expr::Integer(args.len() as _)),
            Engine::list_to_sexp(args),
        ]);
        Err(engine.make_err("application/min-argc", msg, Some(data)))
    } else {
        Ok(())
    }
}

pub fn bad_arg_type(engine: &mut Engine, arg: Gc<Expr>, idx: usize, want: &str) -> Exception {
    let msg = format!("in argument #{}, expected {}", idx, want);
    let data = Engine::list_to_sexp(&[
        Gc::new(Expr::Integer(idx as _)),
        Gc::new(Expr::String(want.to_string())),
        arg,
    ]);
    engine.make_err("application/arg-type", msg, Some(data))
}

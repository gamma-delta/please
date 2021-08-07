//! Ruth's thtandard library.

mod env;
mod eq;
mod funcs;
mod math;
mod pairs_lists;
mod quoting;
mod strings;
mod symbols;
use env::*;
use eq::*;
use funcs::*;
use math::*;
use pairs_lists::*;
use quoting::*;
use strings::*;
use symbols::*;

use std::io::Write;

use gc::{Gc, GcCell};

use crate::{Engine, Expr, Namespace};

pub fn add_thtandard_library(engine: &mut Engine) {
    let thtdlib = engine.thtdlib();

    for (name, special_form) in [
        ("quote", quote as _),
        ("define", define as _),
        ("lambda", lambda_unvariadic as _),
        ("lambda*", lambda_variadic as _),
        ("if", if_ as _),
        ("let", let_ as _),
    ] {
        let symbol = engine.intern_symbol(name);
        let handle = Gc::new(Expr::SpecialForm {
            func: special_form,
            name: symbol,
        });
        thtdlib.borrow_mut().insert(symbol, handle);
    }

    for (name, native_func) in [
        // math
        ("+", add as _),
        ("-", sub as _),
        ("*", mul as _),
        ("/", div as _),
        ("<", lt as _),
        (">", gt as _),
        ("<=", le as _),
        (">=", ge as _),
        ("=", num_eq as _),
        ("and", and as _),
        ("or", or as _),
        ("not", not as _),
        ("xor", xor as _),
        // string
        ("string", to_string as _),
        ("prn", prn as _),
        // list/pair
        ("cons", cons as _),
        ("car", car as _),
        ("cdr", cdr as _),
        // symbols
        ("string->symbol", string2symbol as _),
        ("symbol->string", symbol2string as _),
        // equality
        ("obj-equal?", id_equal as _),
        ("equal?", equal as _),
    ] {
        let symbol = engine.intern_symbol(name);
        let handle = Gc::new(Expr::NativeProcedure {
            func: native_func,
            name: symbol,
        });
        thtdlib.borrow_mut().insert(symbol, handle);
    }

    // Atomic constants that mean nothing other than themselves
    for atom in ["false", "true", "!"] {
        let symbol = engine.intern_symbol(atom);
        // Have the symbol point to itself so it evals to itself
        // it acts like a literal
        thtdlib
            .borrow_mut()
            .insert(symbol, Gc::new(Expr::Symbol(symbol)));
    }

    for (name, thing) in [
        ("ps1", Expr::String(">>> ".to_string())),
        ("ps2", Expr::String("... ".to_string())),
        ("null", Expr::Nil),
    ] {
        let symbol = engine.intern_symbol(name);
        let handle = Gc::new(thing);
        thtdlib.borrow_mut().insert(symbol, handle);
    }

    // and the thtdlib impled in ruth itself
    for source in [
        include_str!("thtd/funcs.ruth"),
        include_str!("thtd/math.ruth"),
    ] {
        engine.read_eval(source).unwrap();
    }
}

// "Contract" functions

fn check_argc(
    engine: &mut Engine,
    args: &[Gc<Expr>],
    min: usize,
    max: usize,
) -> Result<(), Gc<Expr>> {
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
        let data = engine.list_to_sexp(&[
            Gc::new(Expr::Integer(min as _)),
            Gc::new(Expr::Integer(max as _)),
            Gc::new(Expr::Integer(args.len() as _)),
        ]);
        Err(engine.make_err(msg, Some(data)))
    } else {
        Ok(())
    }
}

fn check_min_argc(engine: &mut Engine, args: &[Gc<Expr>], min: usize) -> Result<(), Gc<Expr>> {
    if min > args.len() {
        let msg = format!("expected {} args or more but got {}", min, args.len());
        let data = engine.list_to_sexp(&[
            Gc::new(Expr::Integer(min as _)),
            Gc::new(Expr::Integer(args.len() as _)),
        ]);
        Err(engine.make_err(msg, Some(data)))
    } else {
        Ok(())
    }
}

fn bad_arg_type(engine: &mut Engine, arg: Gc<Expr>, idx: usize, want: &str) -> Gc<Expr> {
    let msg = format!("in argument #{}, expected {}", idx, want);
    let data = engine.list_to_sexp(&[
        Gc::new(Expr::Integer(idx as _)),
        Gc::new(Expr::String(want.to_string())),
        arg,
    ]);
    engine.make_err(msg, Some(data))
}

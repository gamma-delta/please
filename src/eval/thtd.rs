//! Ruth's thtandard library.

use std::io::Write;

use generational_arena::Index;

use crate::{Engine, Expr, Namespace};

pub fn add_thtandard_library(engine: &mut Engine) {
    for (name, func) in [
        // comment to force newline
        ("+", add as _),
        ("define", define as _),
        ("string", to_string as _),
        ("prn", prn as _),
    ] {
        let symbol = engine.intern_symbol(name);
        let handle = engine.insert(Expr::NativeFunction { func, name: symbol });
        engine.thtdlib.insert(symbol, handle);
    }

    for atom in ["false", "true", "!"] {
        let symbol = engine.intern_symbol(atom);
        let handle = engine.insert(Expr::Symbol(symbol));
        // Have the symbol point to itself so true = true = true.
        // it acts like a literal
        engine.thtdlib.insert(symbol, handle);
    }

    for (name, ps) in [("ps1", ">>> "), ("ps2", "... ")] {
        let symbol = engine.intern_symbol(name);
        let handle = engine.insert(Expr::String(ps.to_owned()));
        engine.thtdlib.insert(symbol, handle);
    }

    let null = engine.intern_symbol("null");
    let null_h = engine.insert(Expr::Nil);
    engine.thtdlib.insert(null, null_h);
}

fn add(engine: &mut Engine, env: &mut Namespace, args: &[Index]) -> Expr {
    let evaled = args
        .iter()
        .map(|arg| engine.eval(env, *arg))
        .collect::<Vec<_>>();
    let res: Option<i64> = evaled
        .iter()
        .map(|arg| {
            let expr = engine.get(*arg);
            if let Expr::Integer(int) = expr {
                Some(*int)
            } else {
                None
            }
        })
        .sum();
    if let Some(int) = res {
        Expr::Integer(int)
    } else {
        Expr::Symbol(engine.intern_symbol("!"))
    }
}

fn define(engine: &mut Engine, env: &mut Namespace, args: &[Index]) -> Expr {
    if args.len() != 2 {
        return Expr::Symbol(engine.intern_symbol("!"));
    }

    let name = match engine.get(args[0]) {
        Expr::Symbol(id) => *id,
        _ => return Expr::Symbol(engine.intern_symbol("!")),
    };
    let rhs = engine.eval(env, args[1]);

    env.add(name, rhs);

    Expr::Nil
}

fn to_string(engine: &mut Engine, env: &mut Namespace, args: &[Index]) -> Expr {
    if args.len() != 1 {
        return Expr::Symbol(engine.intern_symbol("!"));
    }
    let evaled = engine.eval(env, args[0]);
    let string = engine.print_expr(evaled);
    Expr::String(string)
}

fn prn(engine: &mut Engine, env: &mut Namespace, args: &[Index]) -> Expr {
    if !(1..=2).contains(&args.len()) {
        return Expr::Symbol(engine.intern_symbol("!"));
    }

    let newline = if args.len() == 2 {
        let check = engine.eval(env, args[1]);
        engine.is_truthy(check)
    } else {
        true
    };

    let to_print = engine.eval(env, args[0]);
    let out = match engine.get(to_print) {
        // show exactly as it is
        Expr::String(it) => it.clone(),
        _ => engine.print_expr(to_print),
    };

    if newline {
        println!("{}", out);
    } else {
        print!("{}", out);
        std::io::stdout().flush().unwrap();
    }

    Expr::Nil
}

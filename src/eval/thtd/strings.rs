//! Messing with strings.
use std::ops::Bound;

use super::*;

pub fn to_string(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    let mut out = String::new();
    for expr in args {
        out.push_str(&engine.print_expr(expr.to_owned()));
    }
    Gc::new(Expr::String(out))
}

pub fn string_slice(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 2, 3) {
        return e;
    }

    let string = match &*args[0] {
        Expr::String(s) => s.as_str(),
        _ => return bad_arg_type(engine, args[0].to_owned(), 0, "string"),
    };

    let start = args[1].to_owned();
    let start = match &*start {
        Expr::Integer(i) if *i >= 0 => *i as usize,
        _ if engine.is_truthy(start.to_owned()) => 0,
        _ => return bad_arg_type(engine, start, 1, "positive int or falsy"),
    };
    let end = args[2].to_owned();
    let end = match &*end {
        Expr::Integer(i) if *i >= 0 => *i as usize,
        _ if engine.is_truthy(end.to_owned()) => string.len(),
        _ => return bad_arg_type(engine, end, 2, "positive int or falsy"),
    };

    if !string.is_char_boundary(start) {
        return engine.make_err(format!("{} is not on a char boundary", start), None);
    }

    // etc, other error checking...
    todo!()
}

pub fn prn(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 1, 2) {
        return e;
    }

    let newline = if let Some(check) = args.get(1) {
        engine.is_truthy(check.to_owned())
    } else {
        true
    };

    let out = engine.print_expr(args[0].clone());

    if newline {
        println!("{}", out);
    } else {
        print!("{}", out);
        std::io::stdout().flush().unwrap();
    }

    Gc::new(Expr::Nil)
}

//! Messing with the environment and namespaces.

use super::*;
use crate::eval::TailRec;

pub fn define(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    if let Err(e) = check_min_argc(engine, args, 2) {
        return TailRec::Exit(e);
    }

    let first = args[0].to_owned();
    if let Expr::Symbol(id) = &*first {
        if let Err(e) = check_argc(engine, args, 2, 2) {
            return TailRec::Exit(e);
        }

        let evaled = engine.eval(env.to_owned(), args[1].to_owned());
        env.borrow_mut().insert(*id, evaled);
        TailRec::Exit(Gc::new(Expr::Nil))
    } else if let Expr::Pair(name, tail) = &*first {
        // > (define (NAME . TAIL) BODY*) becomes (define NAME (lambda TAIL BODY*))
        // --alwinfy
        // construct a sex expression and just have the eval do it for us
        let lambda_name = engine.intern_symbol("lambda");
        let mut lambda_list = vec![Gc::new(Expr::Symbol(lambda_name)), tail.to_owned()];
        lambda_list.extend_from_slice(&args[1..]);
        let lambda_sexpr = engine.list_to_sexp(&lambda_list);

        if !matches!(&**name, Expr::Symbol(..)) {
            return TailRec::Exit(bad_arg_type(engine, first, 0, "(symbol, any)"));
        }

        let define_name = engine.intern_symbol("define");
        let define_list = &[
            Gc::new(Expr::Symbol(define_name)),
            name.to_owned(),
            lambda_sexpr,
        ];
        TailRec::TailRecur(engine.list_to_sexp(define_list), env)
    } else {
        TailRec::Exit(bad_arg_type(engine, args[0].clone(), 0, "symbol"))
    }
}

pub fn let_(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    /*
    (let (
        [key val]
        [key2 val2]) body body body)
    */

    if let Err(ono) = check_min_argc(engine, args, 2) {
        return TailRec::Exit(ono);
    }

    let inner_env = Gc::new(GcCell::new(Namespace::new(env)));

    let arg_bindings = match engine.sexp_to_list(args[0].to_owned()) {
        Some(it) => it,
        None => {
            return TailRec::Exit(bad_arg_type(
                engine,
                args[0].to_owned(),
                0,
                "list of (symbol expr)",
            ))
        }
    };

    for binding in arg_bindings {
        let bind = match engine.sexp_to_list(binding) {
            Some(it) => it,
            None => {
                return TailRec::Exit(bad_arg_type(
                    engine,
                    args[0].to_owned(),
                    0,
                    "list of (symbol expr)",
                ))
            }
        };

        if bind.len() != 2 {
            return TailRec::Exit(bad_arg_type(
                engine,
                args[0].to_owned(),
                0,
                "list of (symbol expr)",
            ));
        }

        let name = match &*bind[0] {
            Expr::Symbol(it) => it,
            _ => {
                return TailRec::Exit(bad_arg_type(
                    engine,
                    args[0].to_owned(),
                    0,
                    "list of (symbol expr)",
                ))
            }
        };

        let evaled = engine.eval(inner_env.clone(), bind[1].to_owned());
        inner_env.borrow_mut().insert(*name, evaled);
    }

    // Now eval the bodies
    // skip the last for tail positioning
    for body in &args[1..args.len() - 1] {
        engine.eval(inner_env.clone(), body.to_owned());
    }
    TailRec::TailRecur(args.last().unwrap().to_owned(), inner_env)
}

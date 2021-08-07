//! Messing with the environment and namespaces.

use super::*;
use crate::eval::TailRec;

pub fn define(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    if let Err(e) = check_min_argc(engine, args, 2) {
        return TailRec::Exit(e);
    }

    let name = args[0].to_owned();
    if let Expr::Symbol(id) = &*name {
        if let Err(e) = check_argc(engine, args, 2, 2) {
            return TailRec::Exit(e);
        }

        let evaled = engine.eval(env.to_owned(), args[1].to_owned());
        env.borrow_mut().insert(*id, evaled);
        TailRec::Exit(Gc::new(Expr::Nil))
    } else if let Some(lambda) = engine.sexp_to_list(name.clone()) {
        let arg_names = lambda
            .into_iter()
            .map(|expr| {
                if let Expr::Symbol(sym) = *expr {
                    Ok(sym)
                } else {
                    Err(bad_arg_type(engine, name.clone(), 0, "list of symbols"))
                }
            })
            .collect::<Result<Vec<_>, _>>();
        let arg_names = match arg_names {
            Ok(it) => it,
            Err(ono) => return TailRec::Exit(ono),
        };

        let (name, arg_names) = match arg_names.split_first() {
            Some(it) => it,
            None => {
                return TailRec::Exit(engine.make_err(
                    "lambda form of define requires a name for the definition".to_string(),
                    None,
                ))
            }
        };

        let lambda = Expr::Procedure {
            args: arg_names.to_owned(),
            body: args[1..].to_owned(),
            env: env.clone(),
            variadic: false,
        };
        env.borrow_mut().insert(*name, Gc::new(lambda));
        TailRec::Exit(Gc::new(Expr::Nil))
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

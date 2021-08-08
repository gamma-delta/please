//! Defining and composing functions.

use super::*;
use crate::eval::TailRec;

pub fn lambda(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    lambda_macro_inner(engine, env, args, true)
}
pub fn macro_(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    lambda_macro_inner(engine, env, args, false)
}

fn lambda_macro_inner(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
    is_lambda: bool,
) -> TailRec {
    /*
        (lambda
            (args list)
            (body) (body) (body))
    */

    if let Err(err) = check_min_argc(engine, args, 2) {
        return TailRec::Exit(err);
    }

    let args_list = args[0].clone();
    let (args_list, last) = engine.expr_to_improper_list(args_list);

    let vararg_name = match &*last {
        Expr::Nil => None,
        Expr::Symbol(sym) => Some(*sym),
        _ => return TailRec::Exit(bad_arg_type(engine, last, 0, "list of symbols")),
    };

    let args_symbols = args_list
        .into_iter()
        .map(|arg| match &*arg {
            Expr::Symbol(it) => Ok((*it, None)),
            Expr::Pair(car, cdr) => {
                let sym = if let Expr::Symbol(sym) = &**car {
                    *sym
                } else {
                    return Err(bad_arg_type(
                        engine,
                        arg,
                        0,
                        "list of symbols or (symbol default)s",
                    ));
                };
                let default = match engine.sexp_to_list(cdr.to_owned()) {
                    Some(it) => it,
                    None => {
                        return Err(bad_arg_type(
                            engine,
                            arg,
                            0,
                            "list of symbols or (symbol default)s",
                        ))
                    }
                };
                let default = match default.as_slice() {
                    [it] => it.to_owned(),
                    _ => {
                        return Err(bad_arg_type(
                            engine,
                            arg,
                            0,
                            "list of symbols or (symbol default)s",
                        ))
                    }
                };
                let default = engine.eval(env.clone(), default);
                Ok((sym, Some(default)))
            }
            _ => Err(bad_arg_type(
                engine,
                arg,
                0,
                "list of symbols or (symbol default)s",
            )),
        })
        .collect::<Result<Vec<_>, _>>();
    let mut args_symbols = match args_symbols {
        Ok(it) => it,
        Err(ono) => return TailRec::Exit(ono),
    };
    if let Some(last) = vararg_name {
        args_symbols.push((last, None));
    }

    let body = args[1..].to_owned();

    let proc = Expr::Procedure {
        args: args_symbols,
        body,
        env: if is_lambda { Some(env) } else { None }, // close over the calling context
        variadic: vararg_name.is_some(),
    };
    TailRec::Exit(Gc::new(proc))
}

pub fn apply(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_min_argc(engine, args, 1) {
        return ono;
    }

    let mut fnargs = args[1..args.len() - 1].to_owned();
    if let Some(trail) = args.last() {
        let trail = match engine.sexp_to_list(trail.to_owned()) {
            Some(it) => it,
            None => return bad_arg_type(engine, trail.to_owned(), args.len() - 1, "list"),
        };
        fnargs.extend(trail);
    }

    let fnargs = engine.list_to_sexp(&fnargs);
    let full_call = Expr::Pair(args[0].to_owned(), fnargs);
    engine.eval(env, Gc::new(full_call))
}

//! Defining and composing functions.

use itertools::Itertools;

use super::*;
use crate::eval::TailRec;

pub fn lambda(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    lambda_macro_inner(engine, env, args, true)
}
pub fn macro_(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    lambda_macro_inner(engine, env, args, false)
}

fn lambda_macro_inner(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
    is_lambda: bool,
) -> Result<TailRec, Exception> {
    /*
        (lambda
            (args list)
            (body) (body) (body))
    */

    check_min_argc(engine, args, 2)?;

    let args_list = args[0].clone();
    let (args_list, last) = engine.expr_to_improper_list(args_list)?;

    let vararg_name = match &*last {
        Expr::Nil => None,
        Expr::Symbol(sym) => Some(*sym),
        _ => return Err(bad_arg_type(engine, last, 0, "list of symbols")),
    };

    let mut args_symbols = args_list
        .into_iter()
        .map(|arg| match &*arg {
            Expr::Symbol(it) => Ok((*it, None)),
            Expr::Pair(..) | Expr::LazyPair(..) => {
                let (car, cdr) = engine.split_cons(arg.clone())?;
                let sym = if let Expr::Symbol(sym) = &*car {
                    *sym
                } else {
                    return Err(bad_arg_type(
                        engine,
                        arg,
                        0,
                        "list of symbols or (symbol default)s",
                    ));
                };
                let default = match engine.sexp_to_list(cdr)? {
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
                let default = engine.eval_inner(env.clone(), default)?;
                Ok((sym, Some(default)))
            }
            _ => Err(bad_arg_type(
                engine,
                arg,
                0,
                "list of symbols or (symbol default)s",
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;
    if let Some(last) = vararg_name {
        args_symbols.push((last, None));
    }

    let body = args[1..].to_owned();

    let proc = Expr::Procedure {
        args: args_symbols,
        body,
        env: if is_lambda { Some(env) } else { None }, // close over the calling context
        variadic: vararg_name.is_some(),
        // Assume this is a lambda with no name
        name: None,
    };
    Ok(TailRec::Exit(Gc::new(proc)))
}

pub fn apply(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_min_argc(engine, args, 1)?;

    let mut fnargs = if args.len() >= 2 {
        args[1..args.len() - 1].to_owned()
    } else {
        Vec::new()
    };
    if let Some(trail) = args.last() {
        let trail = match engine.sexp_to_list(trail.to_owned())? {
            Some(it) => it,
            None => {
                return Err(bad_arg_type(
                    engine,
                    trail.to_owned(),
                    args.len() - 1,
                    "list",
                ))
            }
        };
        fnargs.extend(trail);
    }

    let quote = Gc::new(Expr::Symbol(engine.intern_symbol("quote")));
    let quote = |e| Engine::list_to_sexp(&[quote.clone(), e]);
    let fnargs = fnargs.into_iter().map(quote).collect::<Vec<_>>();
    let fnargs = Engine::list_to_sexp(&fnargs[..]);
    let full_call = Expr::Pair(quote(args[0].to_owned()), fnargs);
    Ok(TailRec::TailRecur(Gc::new(full_call), env))
}

pub fn macro_expand(
    _engine: &mut Engine,
    _env: Gc<GcCell<Namespace>>,
    _args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    todo!();
    /*
    check_min_argc(engine, args, 1)?;

    // Keep evaling until it's no longer a macro call
    let mut expr = args[0].to_owned();
    let is_macro = |x: &Expr| match x {
        Expr::Procedure { env, .. } => env.is_none(),
        _ => false,
    };
    while is_macro(&*expr) {
        engine.eval_rec(env, args[0].to_owned(), false);
    }
    */
}

/// Return the function as created, as `(args...) bodies...)`
pub fn open_fn(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let (args, body, variadic) = match &*args[0] {
        Expr::Procedure {
            args,
            body,
            variadic,
            ..
        } => (args, body, variadic),
        _ => {
            return Err(bad_arg_type(
                engine,
                args[0].to_owned(),
                1,
                "non-native function",
            ))
        }
    };

    let args = args
        .iter()
        .map(|(sym, default)| {
            let sym = Gc::new(Expr::Symbol(*sym));
            if let Some(default) = default {
                Engine::list_to_sexp(&[sym, default.to_owned()])
            } else {
                sym
            }
        })
        .collect_vec();
    let args = if *variadic {
        // unwrap is ok because variadic fns always have at least 1 arg
        let (end, start) = args.split_last().unwrap();
        Engine::list_to_improper_sexp(start, end.to_owned())
    } else {
        Engine::list_to_sexp(&args)
    };
    let body = Engine::list_to_sexp(body);
    Ok(Gc::new(Expr::Pair(args, body)))
}

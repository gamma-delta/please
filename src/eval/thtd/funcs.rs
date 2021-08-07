//! Defining functions.
use super::*;
use crate::eval::TailRec;

pub fn lambda(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
    variadic: bool,
) -> Result<Gc<Expr>, TailRec> {
    /*
        (lambda
            (args list)
            (body) (body) (body))
    */

    if let Err(err) = check_min_argc(engine, args, 2) {
        return Ok(err);
    }

    let args_list = args[0].clone();
    let args_list = match engine.sexp_to_list(args_list.clone()) {
        Some(it) => it,
        None => {
            return Ok(bad_arg_type(engine, args_list, 0, "list of symbols"));
        }
    };
    let args_symbols = args_list
        .into_iter()
        .map(|arg| match &*arg {
            Expr::Symbol(it) => Ok(*it),
            _ => Err(bad_arg_type(engine, arg, 0, "list of symbols")),
        })
        .collect::<Result<_, _>>();
    let args_symbols = match args_symbols {
        Ok(it) => it,
        Err(ono) => return Ok(ono),
    };

    let body = args[1..].to_owned();

    let proc = Expr::Procedure {
        args: args_symbols,
        body,
        env, // close over the calling context
        variadic,
    };
    Ok(Gc::new(proc))
}

pub fn lambda_unvariadic(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<Gc<Expr>, TailRec> {
    lambda(engine, env, args, false)
}

pub fn lambda_variadic(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<Gc<Expr>, TailRec> {
    lambda(engine, env, args, true)
}

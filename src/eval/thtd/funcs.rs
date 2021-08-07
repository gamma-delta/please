//! Defining functions.
use super::*;

pub fn lambda(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
    variadic: bool,
) -> Gc<Expr> {
    /*
        (lambda
            (args list)
            (body) (body) (body))
    */

    if let Err(err) = check_min_argc(engine, args, 2) {
        return err;
    }

    let args_list = args[0].clone();
    let args_list = match engine.sexp_to_list(args_list.clone()) {
        Some(it) => it,
        None => {
            return bad_arg_type(engine, args_list, 0, "list of symbols");
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
        Err(ono) => return ono,
    };

    let body = args[1..].to_owned();

    let proc = Expr::Procedure {
        args: args_symbols,
        body,
        env, // close over the calling context
        variadic,
    };
    Gc::new(proc)
}

pub fn lambda_unvariadic(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Gc<Expr> {
    lambda(engine, env, args, false)
}

pub fn lambda_variadic(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Gc<Expr> {
    lambda(engine, env, args, true)
}

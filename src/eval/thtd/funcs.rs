//! Defining and composing functions.

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

    let arg_spec = args[0].clone();
    let body = args[1..].to_owned();

    let proc = Expr::Procedure {
        arg_spec,
        body,
        env: if is_lambda { Some(env) } else { None }, // close over the calling context
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

    // this is ok to unwrap cause we just checked argc
    let (func, args) = args.split_first().unwrap();
    engine.apply_inner(env, func.to_owned(), args.to_owned())
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

    let (args, body) = match &*args[0] {
        Expr::Procedure { arg_spec, body, .. } => (arg_spec, body),
        _ => {
            return Err(bad_arg_type(
                engine,
                args[0].to_owned(),
                1,
                "non-native function",
            ))
        }
    };

    let body = Engine::list_to_sexp(body);
    Ok(Gc::new(Expr::Pair(args.to_owned(), body)))
}

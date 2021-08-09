//! Quotes, unquotes, and quasiquotes.
use super::*;
use crate::eval::TailRec;

/// Note this is the "function" quote which should just return its argument.
pub fn quote(
    engine: &mut Engine,
    _env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 1, 1)?;
    Ok(TailRec::Exit(args[0].to_owned()))
}

pub fn quasiquote(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 1, 1)?;

    let arg = args[0].to_owned();
    Ok(TailRec::Exit(quasi_helper(engine, env, arg)?))
}

fn quasi_helper(engine: &mut Engine, env: Gc<GcCell<Namespace>>, arg: Gc<Expr>) -> EvalResult {
    let unquote = engine.find_symbol("unquote").unwrap();
    match &*arg {
        Expr::Pair(car, cdr) => {
            if matches!(&**car, Expr::Symbol(sym) if *sym == unquote) {
                let actual_cdr = match &**cdr {
                    Expr::Pair(car, cdr) => {
                        if let Expr::Nil = &**cdr {
                            car.to_owned()
                        } else {
                            return Err(bad_arg_type(engine, cdr.to_owned(), 1, "1-list"));
                        }
                    }
                    _ => return Err(bad_arg_type(engine, cdr.to_owned(), 1, "1-list")),
                };
                engine.eval_inner(env, actual_cdr)
            } else {
                let car = quasi_helper(engine, env.clone(), car.to_owned())?;
                let cdr = quasi_helper(engine, env, cdr.to_owned())?;
                Ok(Gc::new(Expr::Pair(car, cdr)))
            }
        }
        _ => Ok(arg),
    }
}

pub fn unquote(
    engine: &mut Engine,
    _env: Gc<GcCell<Namespace>>,
    _args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    Err(engine.make_err(
        "quote/direct-unquote",
        "cannot directly call unquote (must use within quasiquote)".to_string(),
        None,
    ))
}

pub fn eval(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    args.iter()
        .map(|expr| engine.eval_inner(env.clone(), expr.to_owned()))
        .last()
        .unwrap()
}

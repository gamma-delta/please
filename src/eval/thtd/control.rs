//! Control flow
use crate::eval::TailRec;

use super::*;

pub fn if_(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 3, 3)?;

    let selector = engine.eval_inner(env.clone(), args[0].to_owned())?;

    Ok(TailRec::TailRecur(
        if engine.is_truthy(selector) {
            args[1].to_owned()
        } else {
            args[2].to_owned()
        },
        env,
    ))
}

pub fn do_(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {

    Ok(match args {
        [first @ .., tail] => {
            for expr in first {
                engine.eval_inner(env.clone(), expr.clone())?;
            }
            TailRec::TailRecur(tail.to_owned(), env)
        },
        [] => TailRec::Exit(Gc::new(Expr::Nil)),
    })
}

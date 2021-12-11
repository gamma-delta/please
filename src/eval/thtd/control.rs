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

pub fn if_let(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 4, 4)?;

    let spec = args[0].to_owned();
    let val = engine.eval_inner(env.clone(), args[1].to_owned())?;

    match engine.destructure_assign(env.to_owned(), spec, val) {
        Ok(bindings) => {
            let bound_env = Namespace::new_with(env, bindings);
            Ok(TailRec::TailRecur(
                args[2].to_owned(),
                Gc::new(GcCell::new(bound_env)),
            ))
        }
        Err(exn) => {
            let exn_name = engine.get_symbol_str(exn.id);
            match exn_name {
                Some(name) if name.starts_with(b"assignment/") => {
                    // The assignment failed, so run the second block
                    Ok(TailRec::TailRecur(args[3].to_owned(), env))
                }
                _ => {
                    // oh no some other error happened somehow
                    Err(exn)
                }
            }
        }
    }
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
        }
        [] => TailRec::Exit(Gc::new(Expr::Nil)),
    })
}

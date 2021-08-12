//! Exception handling and catching.
use crate::{eval::TailRec, EvalSource, Value};

use super::*;

pub fn make_exception(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 3, 3)?;

    let exn_name = match &*args[0] {
        Expr::Symbol(s) => *s,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "symbol")),
    };
    let msg = match &*args[1] {
        Expr::String(s) => s.to_owned(),
        _ => return Err(bad_arg_type(engine, args[1].to_owned(), 1, "string")),
    };
    let data = args[2].to_owned();

    Err(Exception {
        id: exn_name,
        info: msg,
        data,
        call_trace: EvalSource { trace: Vec::new() },
    })
}

pub fn catch(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 1, 1)?;

    let res = engine.eval_inner(env, args[0].to_owned());
    Ok(TailRec::Exit(match res {
        Ok(it) => it,
        Err(ono) => ono.into_expr(engine),
    }))
}

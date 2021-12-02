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
        info: String::from_utf8_lossy(&msg).into_owned(),
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

pub fn with_handler(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 3, 3)?;

    let error = match engine.eval_inner(env.to_owned(), args[2].to_owned()) {
        Ok(it) => return Ok(TailRec::Exit(it)),
        Err(ono) => ono,
    };
    let exn = error.clone().into_expr(engine);
    // Quote it to prevent it from trying to eval `!`
    let quoted = Engine::list_to_sexp(&[Gc::new(Expr::Symbol(engine.intern_symbol("quote"))), exn]);

    let checker = Engine::list_to_sexp(&[args[0].to_owned(), quoted.to_owned()]);
    let checked = engine.eval_inner(env.to_owned(), checker)?;
    if engine.is_truthy(checked) {
        let handler = Engine::list_to_sexp(&[args[1].to_owned(), quoted]);
        Ok(TailRec::TailRecur(handler, env))
    } else {
        Err(error)
    }
}

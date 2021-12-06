//! Random things that I don't know where else to put.

use std::time::{Duration, Instant};

use crate::Value;

use super::*;

pub fn timeit(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_min_argc(engine, args, 1)?;

    let now = Instant::now();
    for (idx, arg) in args.iter().enumerate() {
        let res = engine.eval_inner(env.to_owned(), arg.to_owned());
        let out = match res {
            Err(ono) => Some(ono.into_expr(engine)),
            Ok(val) if idx == args.len() - 1 => Some(val),
            _ => None,
        };
        if let Some(out) = out {
            let delta = now.elapsed();
            let result = Engine::list_to_sexp(&[Gc::new(Expr::Float(delta.as_secs_f64())), out]);
            return Ok(result);
        }
    }

    unreachable!()
}

pub fn exit(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 0, 1)?;
    let msg = if let Some(msg) = args.get(0) {
        engine.print_expr(msg.to_owned())?
    } else {
        String::from("exited")
    };
    // Very professional here
    panic!("{}", msg);
}

pub fn sleep(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    let seconds = Num::from_expr(engine, args[0].to_owned(), 0)?;
    std::thread::sleep(Duration::from_secs_f64(seconds.as_float()));
    Ok(Expr::nil())
}

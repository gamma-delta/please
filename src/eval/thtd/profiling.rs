use std::collections::HashMap;

use hashable::HashableHashMap;

use crate::hash::GcMap;

use super::*;

pub fn start_profiling(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 0, 0)?;

    engine.profiler = Some(HashMap::new());
    Ok(Gc::new(Expr::Nil))
}

pub fn check_profiling(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 0, 0)?;

    Ok(match &engine.profiler {
        Some(profiler) => Gc::new(Expr::Map(profile_to_map(profiler))),
        None => engine.make_bool(false),
    })
}

pub fn stop_profiling(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 0, 0)?;

    Ok(match engine.profiler.take() {
        Some(profiler) => Gc::new(Expr::Map(profile_to_map(&profiler))),
        None => engine.make_bool(false),
    })
}

fn profile_to_map(profile: &HashMap<u64, (u64, f64)>) -> GcMap {
    profile
        .iter()
        .map(|(k, (count, time))| {
            (
                Gc::new(Expr::Symbol(*k)),
                Engine::list_to_sexp(&[
                    Gc::new(Expr::Integer(*count as _)),
                    Gc::new(Expr::Float(*time)),
                ]),
            )
        })
        .collect::<HashableHashMap<_, _>>()
        .into()
}

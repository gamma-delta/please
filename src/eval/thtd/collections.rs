//! doobadoobadoo collection gadgets

use std::ops::Deref;

use itertools::Itertools;

use crate::{hash::GcMap, Value};

use super::*;

pub fn new_map(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    let kv_count = args.len();
    if kv_count % 2 != 0 {
        return Err(engine.make_err("map/new/kv-mismatch", format!("expected an even number of args so there are equally many keys and values, but {} is not even", kv_count), Some(Gc::new(Expr::Integer(kv_count as _)))));
    }

    let mut map = GcMap::new();
    for kv in args.chunks_exact(2) {
        map.insert(kv[0].to_owned(), kv[1].to_owned());
    }

    Ok(Gc::new(Expr::Map(map)))
}

pub fn map_insert_factory(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> EvalResult {
    // get-clobbered, use-env, map, k v k v
    check_min_argc(engine, args, 2)?;

    let get_clobbered = engine.is_truthy(args[0].to_owned());
    let use_env = engine.is_truthy(args[1].to_owned());

    let mut inner = |map: &mut GcMap, args: &[Value]| {
        let kv_count = args.len();
        if kv_count % 2 != 0 {
            return Err(engine.make_err("map/insert/kv-mismatch", format!("expected an even number of trailing args so there are equally many keys and values, but {} is not even", kv_count), Some(Gc::new(Expr::Integer(kv_count as _)))));
        }

        let mut clobbered = Vec::with_capacity(kv_count / 2);
        // systems programming languages need const generics
        // step 1: create rfc#2000
        // step 2: wait for it to stabilize
        // step 3: n e v e r stabilize anything using them
        // 2021 "array_chunks" incident
        for kv in args.chunks_exact(2) {
            let clob = map.insert(kv[0].to_owned(), kv[1].to_owned());
            if let Some(clob) = clob {
                clobbered.push(clob);
            }
        }

        Ok(clobbered)
    };

    if !use_env {
        if let Some(expr) = args.get(2) {
            if let Expr::Map(m) = &**expr {
                let mut map = m.clone();
                let clobbered = inner(&mut map, &args[3..])?;

                Ok(Gc::new(if get_clobbered {
                    let tail = Engine::list_to_sexp(&clobbered);
                    Expr::Pair(Gc::new(Expr::Map(map)), tail)
                } else {
                    Expr::Map(map)
                }))
            } else {
                Err(bad_arg_type(engine, args[0].to_owned(), 2, "map"))
            }
        } else {
            Err(check_min_argc(engine, args, 3).unwrap_err())
        }
    } else {
        let env = env.borrow();
        let ctx = env.get_context();
        if let Some(ctx) = ctx {
            let mut lock = ctx.borrow_mut();
            if let Expr::Map(ref mut map) = *lock {
                let clobbered = inner(map, &args[2..])?;
                Ok(if get_clobbered {
                    Engine::list_to_sexp(&clobbered)
                } else {
                    Expr::nil()
                })
            } else {
                Err(bad_ctx_type(engine, Gc::new((*lock).clone()), "map"))
            }
        } else {
            Err(bad_no_ctx(engine, "map"))
        }
    }
}

pub fn map_get_factory(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> EvalResult {
    // use-env map k k
    check_min_argc(engine, args, 1)?;

    let use_env = engine.is_truthy(args[0].to_owned());

    let inner = |map: &GcMap, args: &[Value]| {
        let mut all_found = Vec::with_capacity(args.len());
        // systems programming languages need const generics
        // step 1: create rfc#2000
        // step 2: wait for it to stabilize
        // step 3: n e v e r stabilize anything using them
        // 2021 "array_chunks" incident
        for k in args {
            let found = map.get(k);
            all_found.push(if let Some(found) = found {
                found.to_owned()
            } else {
                Expr::bool(false)
            })
        }

        Ok(all_found)
    };

    if !use_env {
        if let Some(expr) = args.get(1) {
            if let Expr::Map(m) = &**expr {
                let mut map = m.clone();
                let found = inner(&mut map, &args[2..])?;

                Ok(Engine::list_to_sexp(&found))
            } else {
                Err(bad_arg_type(engine, args[1].to_owned(), 1, "map"))
            }
        } else {
            Err(check_min_argc(engine, args, 2).unwrap_err())
        }
    } else {
        let env = env.borrow();
        let ctx = env.get_context();
        if let Some(ctx) = ctx {
            let lock = ctx.borrow();
            if let Expr::Map(ref map) = *lock {
                let found = inner(map, &args[1..])?;
                Ok(Engine::list_to_sexp(&found))
            } else {
                Err(bad_ctx_type(engine, Gc::new((*lock).clone()), "map"))
            }
        } else {
            Err(bad_no_ctx(engine, "map"))
        }
    }
}

pub fn map_remove_factory(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> EvalResult {
    // get-clobbered, use-env, map, k v k v
    check_min_argc(engine, args, 2)?;

    let get_clobbered = engine.is_truthy(args[0].to_owned());
    let use_env = engine.is_truthy(args[1].to_owned());

    let inner = |map: &mut GcMap, args: &[Value]| {
        let mut all_found = Vec::with_capacity(args.len());
        // systems programming languages need const generics
        // step 1: create rfc#2000
        // step 2: wait for it to stabilize
        // step 3: n e v e r stabilize anything using them
        // 2021 "array_chunks" incident
        for k in args {
            let found = map.remove(k);
            if let Some(found) = found {
                all_found.push(found);
            }
        }

        Ok(all_found)
    };

    if !use_env {
        if let Some(expr) = args.get(2) {
            if let Expr::Map(m) = &**expr {
                let mut map = m.clone();
                let clobbered = inner(&mut map, &args[3..])?;

                Ok(Gc::new(if get_clobbered {
                    let tail = Engine::list_to_sexp(&clobbered);
                    Expr::Pair(Gc::new(Expr::Map(map)), tail)
                } else {
                    Expr::Map(map)
                }))
            } else {
                Err(bad_arg_type(engine, args[0].to_owned(), 2, "map"))
            }
        } else {
            Err(check_min_argc(engine, args, 3).unwrap_err())
        }
    } else {
        let env = env.borrow();
        let ctx = env.get_context();
        if let Some(ctx) = ctx {
            let mut lock = ctx.borrow_mut();
            if let Expr::Map(ref mut map) = *lock {
                let clobbered = inner(map, &args[2..])?;
                Ok(if get_clobbered {
                    Engine::list_to_sexp(&clobbered)
                } else {
                    Expr::nil()
                })
            } else {
                Err(bad_ctx_type(engine, Gc::new((*lock).clone()), "map"))
            }
        } else {
            Err(bad_no_ctx(engine, "map"))
        }
    }
}

pub fn map_len(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let map = if let Expr::Map(m) = &*args[0] {
        m
    } else {
        return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map"));
    };

    Ok(Gc::new(Expr::Integer(map.len() as _)))
}

pub fn map2list(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let map = match &*args[0] {
        Expr::Map(m) => m,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };
    let kvs = map
        .iter()
        .map(|(k, v)| Gc::new(Expr::Pair(k.to_owned(), v.to_owned())))
        .collect_vec();

    Ok(Engine::list_to_sexp(&kvs))
}

//! doobadoobadoo collection gadgets

use std::collections::HashMap;

use itertools::Itertools;

use crate::Value;

use super::*;

pub fn new_map(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 0, 0)?;
    Ok(Gc::new(Expr::Map(HashMap::new())))
}

pub fn map_insert(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_min_argc(engine, args, 1)?;

    let mut map = match &*args[0] {
        Expr::Map(m) => m.to_owned(),
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };

    let kv_count = args.len() - 1;
    if kv_count % 2 != 0 {
        return Err(engine.make_err("map/insert-kv-mismatch", format!("expected an even number of trailing args so there are equally many keys and values, but {} is not even", kv_count), Some(Gc::new(Expr::Integer(kv_count as _)))));
    }

    let mut clobbered = Vec::with_capacity(kv_count / 2);
    // systems programming languages need const generics
    // step 1: create rfc#2000
    // step 2: wait for it to stabilize
    // step 3: n e v e r stabilize anything using them
    // 2021 "array_chunks" incident
    for kv in args[1..].chunks_exact(2) {
        let k = engine.to_hashable(kv[0].to_owned())?;
        let clob = map.insert(k, kv[1].to_owned());
        clobbered.push(clob.unwrap_or_else(|| engine.make_bool(false)));
    }

    let new_map = Gc::new(Expr::Map(map));
    let tail = Engine::list_to_sexp(&clobbered);
    Ok(Gc::new(Expr::Pair(new_map, tail)))
}

pub fn map_get(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_min_argc(engine, args, 2)?;

    let map = match &*args[0] {
        Expr::Map(m) => m,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };

    let mut found = Vec::with_capacity(args.len() - 1);
    for k in &args[1..] {
        let k = engine.to_hashable(k.to_owned())?;
        let elt = map.get(&k);
        found.push(elt.cloned().unwrap_or_else(|| engine.make_bool(false)));
    }

    Ok(if found.len() == 1 {
        found[0].to_owned()
    } else {
        Engine::list_to_sexp(&found)
    })
}

pub fn map_remove(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_min_argc(engine, args, 2)?;

    let mut map = match &*args[0] {
        Expr::Map(m) => m.to_owned(),
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };

    let mut removed = Vec::with_capacity(args.len() - 1);
    for k in &args[1..] {
        let k = engine.to_hashable(k.to_owned())?;
        let elt = map.remove(&k);
        removed.push(elt.unwrap_or_else(|| engine.make_bool(false)));
    }

    let new_map = Gc::new(Expr::Map(map));
    let tail = Engine::list_to_sexp(&removed);
    Ok(Gc::new(Expr::Pair(new_map, tail)))
}

pub fn map2list(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let map = match &*args[0] {
        Expr::Map(m) => m,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };

    let kvs = map
        .iter()
        .map(|(k, v)| Gc::new(Expr::Pair(engine.from_hashable(k), v.to_owned())))
        .collect_vec();

    Ok(Engine::list_to_sexp(&kvs))
}

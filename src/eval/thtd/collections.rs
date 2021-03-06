//! doobadoobadoo collection gadgets

use std::borrow::Cow;

use itertools::Itertools;

use crate::{hash::GcMap, Value};

use super::*;

fn get_read_map<'x>(
    engine: &mut Engine,
    expr: &'x Gc<Expr>,
    idx: usize,
) -> Result<Cow<'x, GcMap>, Exception> {
    Ok(match &**expr {
        Expr::Map(m) => Cow::Borrowed(m),
        Expr::Transient(t) => {
            let borrowed = borrow_transient(engine, t)?;
            let t = borrowed.as_ref().unwrap();
            match &**t {
                // todo: can we not clone this
                Expr::Map(map) => Cow::Owned(map.clone()),
                _ => return Err(bad_arg_type(engine, expr.to_owned(), idx, "map")),
            }
        }
        _ => return Err(bad_arg_type(engine, expr.to_owned(), idx, "map")),
    })
}

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

pub fn map_get(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_min_argc(engine, args, 2)?;

    let map = get_read_map(engine, &args[0], 0)?;

    let mut found = Vec::with_capacity(args.len() - 1);
    for k in &args[1..] {
        let elt = map.get(k);
        found.push(elt.cloned().unwrap_or_else(|| engine.make_bool(false)));
    }

    Ok(if found.len() == 1 {
        found[0].to_owned()
    } else {
        Engine::list_to_sexp(&found)
    })
}

pub fn map_contains(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_min_argc(engine, args, 1)?;

    let map = get_read_map(engine, &args[0], 0)?;
    let contains = args[1..]
        .iter()
        .map(|k| engine.make_bool(map.contains_key(k)))
        .collect_vec();

    Ok(if let [expr] = contains.as_slice() {
        expr.to_owned()
    } else {
        Engine::list_to_sexp(&contains)
    })
}

pub fn map_len(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let map = get_read_map(engine, &args[0], 0)?;
    Ok(Gc::new(Expr::Integer(map.len() as _)))
}

pub fn map2list(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let map = get_read_map(engine, &args[0], 0)?;
    let kvs = map
        .iter()
        .map(|(k, v)| Engine::list_to_sexp(&[k.to_owned(), v.to_owned()]))
        .collect_vec();

    Ok(Engine::list_to_sexp(&kvs))
}

pub fn map_insert(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    map_insert_inner(engine, env, args, false)
}

pub fn map_insert_clobbered(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> EvalResult {
    map_insert_inner(engine, env, args, true)
}

fn map_insert_inner(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Value],
    get_clobbered: bool,
) -> EvalResult {
    check_min_argc(engine, args, 1)?;

    let mut map = match &*args[0] {
        Expr::Map(m) => m.to_owned(),
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };

    let kv_count = args.len() - 1;
    if kv_count % 2 != 0 {
        return Err(engine.make_err("map/insert/kv-mismatch", format!("expected an even number of trailing args so there are equally many keys and values, but {} is not even", kv_count - 1), Some(Gc::new(Expr::Integer(kv_count as _)))));
    }

    let mut clobbered = get_clobbered.then(|| Vec::with_capacity(kv_count / 2));
    // systems programming languages need const generics
    // step 1: create rfc#2000
    // step 2: wait for it to stabilize
    // step 3: n e v e r stabilize anything using them
    // 2021 "array_chunks" incident
    for kv in args[1..].chunks_exact(2) {
        let clob = map.insert(kv[0].to_owned(), kv[1].to_owned());
        if let Some(ref mut clobbered) = clobbered {
            clobbered.push(clob.unwrap_or_else(|| engine.make_bool(false)));
        }
    }

    Ok(Gc::new(if let Some(clobbered) = clobbered {
        let tail = Engine::list_to_sexp(&clobbered);
        Expr::Pair(Gc::new(Expr::Map(map)), tail)
    } else {
        Expr::Map(map)
    }))
}

pub fn map_remove(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    map_remove_inner(engine, env, args, false)
}
pub fn map_remove_clobbered(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Value],
) -> EvalResult {
    map_remove_inner(engine, env, args, true)
}
fn map_remove_inner(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Value],
    get_clobbered: bool,
) -> EvalResult {
    check_min_argc(engine, args, 2)?;

    let mut map = match &*args[0] {
        Expr::Map(m) => m.to_owned(),
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "map")),
    };

    let mut removed = get_clobbered.then(|| Vec::with_capacity(args.len() - 1));
    for k in &args[1..] {
        let elt = map.remove(k);
        if let Some(ref mut removed) = removed {
            removed.push(elt.unwrap_or_else(|| engine.make_bool(false)));
        }
    }

    Ok(Gc::new(if let Some(removed) = removed {
        let tail = Engine::list_to_sexp(&removed);
        Expr::Pair(Gc::new(Expr::Map(map)), tail)
    } else {
        Expr::Map(map)
    }))
}

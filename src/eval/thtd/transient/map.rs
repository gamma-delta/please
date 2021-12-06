use super::super::*;

pub fn insert(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    map_insert_inner(engine, env, args, false)
}

pub fn insert_clobbered(
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

    let trans = match &*args[0] {
        Expr::Transient(t) => t,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient map")),
    };
    let mut taken = take_transient(engine, trans)?;
    let mut map = match &mut taken {
        Expr::Map(m) => std::mem::take(m),
        ono => {
            return Err(bad_arg_type(
                engine,
                Gc::new(ono.clone()),
                0,
                "transient map",
            ))
        }
    };

    let kv_count = args.len() - 1;
    if kv_count % 2 != 0 {
        return Err(engine.make_err("map/insert!/kv-mismatch", format!("expected an even number of trailing args so there are equally many keys and values, but {} is not even", kv_count - 1), Some(Gc::new(Expr::Integer(kv_count as _)))));
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

    // Wrap it back up
    let trans = Expr::transient(Expr::Map(map));

    Ok(if let Some(clobbered) = clobbered {
        let tail = Engine::list_to_sexp(&clobbered);
        Expr::pair(trans, tail)
    } else {
        trans
    })
}

pub fn remove(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    map_remove_inner(engine, env, args, false)
}
pub fn remove_clobbered(
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

    let trans = match &*args[0] {
        Expr::Transient(t) => t,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient map")),
    };
    let mut taken = take_transient(engine, trans)?;
    let mut map = match &mut taken {
        Expr::Map(m) => std::mem::take(m),
        ono => {
            return Err(bad_arg_type(
                engine,
                Gc::new(ono.clone()),
                0,
                "transient map",
            ))
        }
    };

    let mut removed = get_clobbered.then(|| Vec::with_capacity(args.len() - 1));
    for k in &args[1..] {
        let elt = map.remove(k);
        if let Some(ref mut removed) = removed {
            removed.push(elt.unwrap_or_else(|| engine.make_bool(false)));
        }
    }

    let trans = Expr::transient(Expr::Map(map));
    Ok(if let Some(removed) = removed {
        let tail = Engine::list_to_sexp(&removed);
        Expr::pair(trans, tail)
    } else {
        trans
    })
}

pub fn clear(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let trans = match &*args[0] {
        Expr::Transient(t) => t,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient map")),
    };
    let mut taken = take_transient(engine, trans)?;
    let mut map = match &mut taken {
        Expr::Map(m) => std::mem::take(m),
        ono => {
            return Err(bad_arg_type(
                engine,
                Gc::new(ono.clone()),
                0,
                "transient map",
            ))
        }
    };

    map.clear();

    Ok(Expr::transient(Expr::Map(map)))
}

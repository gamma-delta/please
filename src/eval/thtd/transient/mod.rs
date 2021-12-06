use super::*;

pub mod map;

pub fn new(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    Ok(Gc::new(Expr::Transient(GcCell::new(Some(Box::new(
        (*args[0]).clone(),
    ))))))
}

pub fn persist(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    match &*args[0] {
        Expr::Transient(t) => take_transient(engine, t).map(Gc::new),
        _ => Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient")),
    }
}

pub fn has_value(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    match &*args[0] {
        Expr::Transient(t) => {
            let lock = t.borrow();
            Ok(Expr::bool(lock.is_some()))
        }
        _ => Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient")),
    }
}

/// Update a transient with a *non-transient* value
pub fn update(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;
    let trans = match &*args[0] {
        Expr::Transient(t) => t,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient")),
    };

    let mut lock = trans.borrow_mut();
    let prev = lock.replace(Box::new((*args[1]).clone()));
    Ok(if let Some(prev) = prev {
        Gc::new((*prev).clone())
    } else {
        Expr::bool(false)
    })
}

/// Update a transient with a *transient* value
pub fn replace(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;
    let replacee = match &*args[0] {
        Expr::Transient(t) => t,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "transient")),
    };
    let replacer = match &*args[1] {
        Expr::Transient(t) => t,
        _ => return Err(bad_arg_type(engine, args[1].to_owned(), 1, "transient")),
    };
    let replacer = take_transient(engine, replacer)?;

    let mut lock = replacee.borrow_mut();
    let prev = lock.replace(Box::new(replacer));
    Ok(if let Some(prev) = prev {
        Gc::new((*prev).clone())
    } else {
        Expr::bool(false)
    })
}

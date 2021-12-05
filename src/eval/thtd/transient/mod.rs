use super::*;

pub mod map;

pub fn new(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    Ok(Gc::new(Expr::Transient(GcCell::new(Some(Box::new(
        (*args[0]).clone(),
    ))))))
}

pub fn persist_bang(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    match &*args[0] {
        Expr::Transient(t) => take_transient(engine, t.to_owned()).map(Gc::new),
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

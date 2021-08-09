//! Constructing and working with pairs and lists.
use super::*;
use crate::TailRec;

pub fn cons(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    Ok(Gc::new(Expr::Pair(args[0].to_owned(), args[1].to_owned())))
}

pub fn lazy_cons(engine: &mut Engine, ns: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Result<TailRec, Exception> {
    check_argc(engine, args, 2, 2)?;

    Ok(TailRec::Exit(Gc::new(Expr::LazyPair(GcCell::new((args[0].to_owned(), false)), GcCell::new((args[1].to_owned(), false)), ns))))
}

pub fn car(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let (car, _) = match engine.split_cons(args[0].clone()) {
        Some((car, cdr)) => (car, cdr),
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "pair")),
    };
    Ok(car.clone())
}

pub fn cdr(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let (_, cdr) = match engine.split_cons(args[0].clone()) {
        Some((car, cdr)) => (car, cdr),
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "pair")),
    };
    Ok(cdr.clone())
}

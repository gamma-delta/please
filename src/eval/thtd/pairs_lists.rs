//! Constructing and working with pairs and lists.
use super::*;

pub fn cons(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    Ok(Gc::new(Expr::Pair(args[0].to_owned(), args[1].to_owned())))
}

pub fn car(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let (car, _) = match &*args[0] {
        Expr::Pair(car, cdr) => (car, cdr),
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "pair")),
    };
    Ok(car.clone())
}

pub fn cdr(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let (_, cdr) = match &*args[0] {
        Expr::Pair(car, cdr) => (car, cdr),
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "pair")),
    };
    Ok(cdr.clone())
}

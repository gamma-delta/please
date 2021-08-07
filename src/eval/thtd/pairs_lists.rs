//! Constructing and working with pairs and lists.
use super::*;

pub fn cons(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 2, 2) {
        return e;
    }

    Gc::new(Expr::Pair(args[0].to_owned(), args[1].to_owned()))
}

pub fn car(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 1, 1) {
        return e;
    }

    let (car, _) = match &*args[0] {
        Expr::Pair(car, cdr) => (car, cdr),
        _ => return bad_arg_type(engine, args[0].clone(), 0, "pair"),
    };
    car.clone()
}

pub fn cdr(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 1, 1) {
        return e;
    }

    let (_, cdr) = match &*args[0] {
        Expr::Pair(car, cdr) => (car, cdr),
        _ => return bad_arg_type(engine, args[0].clone(), 0, "pair"),
    };
    cdr.clone()
}

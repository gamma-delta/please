//! Equality checks.

use super::*;

/// Return whether things are the same object
pub fn id_equal(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    let (lhs, rest) = args.split_first().unwrap();
    for rhs in rest {
        if !Gc::ptr_eq(lhs, rhs) {
            return Ok(engine.make_bool(false));
        }
    }
    Ok(engine.make_bool(true))
}

fn smart_equal<L, R>(engine: &mut Engine, lhs: L, rhs: R) -> Result<bool, Exception>
where
    L: AsRef<Expr>,
    R: AsRef<Expr>,
{
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Pair(..) | Expr::LazyPair(..), Expr::Pair(..) | Expr::LazyPair(..)) => {
            let (lhs_car, lhs_cdr) = engine.split_cons_verb(lhs)?.unwrap();
            let (rhs_car, rhs_cdr) = engine.split_cons_verb(rhs)?.unwrap();
            smart_equal(engine, lhs_car, rhs_car)? && smart_equal(engine, lhs_cdr, rhs_cdr)?
        }
        (Expr::Transient(lhs), Expr::Transient(rhs)) => {
            let lhs_lock = lhs.borrow();
            let rhs_lock = rhs.borrow();
            match (&*lhs_lock, &*rhs_lock) {
                (None, None) => true,
                (Some(..), None) | (None, Some(..)) => false,
                (Some(l), Some(r)) => smart_equal(engine, l, r)?,
            }
        }
        (Expr::Map(lhs), Expr::Map(rhs)) => {
            if lhs.len() != rhs.len() {
                false
            } else {
                for (k, lhs_v) in lhs.iter() {
                    if let Some(rhs_v) = rhs.get(k) {
                        if !smart_equal(engine, lhs_v, rhs_v)? {
                            return Ok(false);
                        }
                    }
                }
                true
            }
        }
        (lhs, rhs) => lhs == rhs,
    })
}

/// return whether things are equal w/o type juggling
pub fn equal(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    let (lhs, rest) = args.split_first().unwrap();
    for rhs in rest {
        // commutativity (?) means if a == b and a == c, b == c
        // so we just check the first against each
        if !smart_equal(engine, lhs.to_owned(), rhs.to_owned())? {
            return Ok(engine.make_bool(false));
        }
    }
    Ok(engine.make_bool(true))
}

pub fn typeof_(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let sym = args[0].type_name();
    Ok(Gc::new(Expr::Symbol(engine.intern_symbol(sym))))
}

macro_rules! predicates {
    ($name:ident) => {
        pub fn $name(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
            check_argc(engine, args, 1, 1)?;
            Ok(engine.make_bool(args[0].$name()))
        }
    };
    ($head:tt $($tail:tt)+) => {
        predicates! { $head }
        predicates! { $($tail)* }
    };
}

predicates! {
    is_pair
    is_number
    is_exact
    is_inexact
    is_nil
    is_string
    is_bool
    is_symbol
    is_map
    is_callable
    is_procedure
    is_macro
    is_transient
}

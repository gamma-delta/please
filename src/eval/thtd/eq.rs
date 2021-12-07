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

/// return whether things are equal w/o type juggling
pub fn equal(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    let (lhs, rest) = args.split_first().unwrap();
    for rhs in rest {
        // commutativity (?) means if a == b and a == c, b == c
        // so we just check the first against each
        if lhs != rhs {
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

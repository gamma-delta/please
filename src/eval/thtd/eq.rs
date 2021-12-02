//! Equality checks.

use super::*;

/// Return whether things are the same object
pub fn id_equal(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    let (lhs, rest) = args.split_first().unwrap();
    let mut all_eq = true;
    for rhs in rest {
        all_eq = Gc::ptr_eq(lhs, rhs);
        if !all_eq {
            break;
        }
    }
    Ok(engine.make_bool(all_eq))
}

/// return whether two things are equal w/o type juggling
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

macro_rules! predicates {
    (($name:ident $pat:pat)) => {
        #[allow(unused_parens)]
        pub fn $name(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
            check_argc(engine, args, 1, 1)?;
            Ok(engine.make_bool(matches!(&*args[0], $pat)))
        }
    };
    ($head:tt $($tail:tt)+) => {
        predicates! { $head }
        predicates! { $($tail)* }
    };
}

predicates! {
    (is_pair (Expr::Pair(..) | Expr::LazyPair(..)))
    (is_number (Expr::Integer(_) | Expr::Float(_)))
    (is_exact Expr::Integer(..))
    (is_inexact Expr::Float(..))
    (is_nil Expr::Nil)
    (is_string Expr::String(_))
    (is_symbol Expr::Symbol(_))
    (is_map Expr::Map(_))
    (is_callable (Expr::NativeProcedure { .. } | Expr::SpecialForm { .. } | Expr::Procedure { .. }))
    (is_procedure (Expr::NativeProcedure { .. } | Expr::Procedure { env: Some(_), .. }))
    (is_macro (Expr::SpecialForm { .. } | Expr::Procedure { env: None, .. }))
}

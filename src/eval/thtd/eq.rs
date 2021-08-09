//! Equality checks.

use super::*;

/// return whether two things are the same object
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
    let mut all_eq = true;
    for rhs in rest {
        // commutativity (?) means if a == b and a == c, b == c
        // so we just check the first against each
        all_eq = check_equal(engine, lhs.to_owned(), rhs.to_owned());
        if !all_eq {
            break;
        }
    }
    Ok(engine.make_bool(all_eq))
}

fn check_equal(engine: &mut Engine, lhs: Gc<Expr>, rhs: Gc<Expr>) -> bool {
    match (&*lhs, &*rhs) {
        (Expr::Integer(l), Expr::Integer(r)) => l == r,
        (Expr::Float(l), Expr::Float(r)) => (l - r).abs() < 1e-10,
        (Expr::String(l), Expr::String(r)) => l == r,
        (Expr::Symbol(l), Expr::Symbol(r)) => l == r,
        (Expr::Nil, Expr::Nil) => true,
        (Expr::SpecialForm { .. }, Expr::SpecialForm { .. })
        | (Expr::NativeProcedure { .. }, Expr::NativeProcedure { .. })
        | (Expr::Procedure { .. }, Expr::Procedure { .. }) => Gc::ptr_eq(&lhs, &rhs),

        (Expr::Pair(ll, lr), Expr::Pair(rl, rr)) => {
            check_equal(engine, ll.to_owned(), rl.to_owned())
                && check_equal(engine, lr.to_owned(), rr.to_owned())
        }
        _ => false,
    }
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
    (is_pair Expr::Pair(..))
    (is_number (Expr::Integer(_) | Expr::Float(_)))
    (is_exact Expr::Integer(..))
    (is_inexact Expr::Float(..))
    (is_nil Expr::Nil)
    (is_string Expr::String(_))
    (is_symbol Expr::Symbol(_))
    (is_callable (Expr::NativeProcedure { .. } | Expr::SpecialForm { .. } | Expr::Procedure { .. }))
    (is_procedure (Expr::NativeProcedure { .. } | Expr::Procedure { env: Some(_), .. }))
    (is_macro (Expr::SpecialForm { .. } | Expr::Procedure { env: None, .. }))
}

use super::*;

macro_rules! predicates {
    (@impl $($tail:tt)+) => {
        impl Expr {
            predicates! {$($tail)*}
        }
    };
    (($name:ident $pat:pat)) => {
        #[allow(unused_parens)]
        pub fn $name(&self) -> bool {
            matches!(self, $pat)
        }

    };
    ($head:tt $($tail:tt)+) => {
        predicates! { $head }
        predicates! { $($tail)* }
    };
}

predicates! {
    @impl
    (is_pair (Expr::Pair(..) | Expr::LazyPair(..)))
    (is_number (Expr::Integer(_) | Expr::Float(_)))
    (is_exact Expr::Integer(..))
    (is_inexact Expr::Float(..))
    (is_nil Expr::Nil)
    (is_string Expr::String(_))
    (is_symbol Expr::Symbol(_))
    (is_bool Expr::Bool(_))
    (is_map Expr::Map(_))
    (is_callable (Expr::NativeProcedure { .. } | Expr::SpecialForm { .. } | Expr::Procedure { .. }))
    (is_procedure (Expr::NativeProcedure { .. } | Expr::Procedure { env: Some(_), .. }))
    (is_macro (Expr::SpecialForm { .. } | Expr::Procedure { env: None, .. }))
    (is_transient (Expr::Transient(_)))
}

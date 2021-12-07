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

impl Expr {
    pub fn type_name(&self) -> &'static str {
        match self {
            Expr::Integer(_) => "integer",
            Expr::Float(_) => "float",
            Expr::Bool(_) => "bool",
            Expr::String(_) => "string",
            Expr::Symbol(_) => "symbol",
            Expr::Pair(_, _) => "pair",
            Expr::LazyPair(_, _, _) => "lazy-pair",
            Expr::Nil => "nil",
            Expr::SpecialForm { .. } => "special-form",
            Expr::NativeProcedure { .. } => "native-procedure",
            Expr::Procedure { .. } => "procedure",
            Expr::Map(_) => "map",
            Expr::Transient(_) => "transient",
        }
    }
}

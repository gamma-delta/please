//! Hashing and equality testing for things.
use std::hash::Hash;

use super::*;

/// Expressions you can hash.
///
/// Because NaN was a mistake, all NaN are considered equal to each other.
/// I don't care what the IEEE says. Shut up.
#[derive(Debug, Clone, Trace, Finalize)]
pub enum HashEqExpr {
    Integer(i64),
    Float(f64),
    /// Yes this is cloned fite me
    String(String),
    Symbol(Symbol),
    Nil,
}

impl PartialEq for HashEqExpr {
    fn eq(&self, other: &Self) -> bool {
        use HashEqExpr::*;
        match (self, other) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => {
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

impl Eq for HashEqExpr {}

impl Hash for HashEqExpr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            HashEqExpr::Integer(x) => state.write_i64(*x),
            HashEqExpr::Float(x) => state.write_u64(x.to_bits()),
            HashEqExpr::String(x) => x.hash(state),
            HashEqExpr::Symbol(x) => state.write_u64(*x),
            HashEqExpr::Nil => {}
        }
    }
}

impl Engine {
    /// Turn an expression into a hashable equivalent.
    pub fn to_hashable(&mut self, x: Gc<Expr>) -> Result<HashEqExpr, Exception> {
        Ok(match &*x {
            Expr::Integer(x) => HashEqExpr::Integer(*x),
            Expr::Float(x) => HashEqExpr::Float(*x),
            Expr::String(x) => HashEqExpr::String(x.to_owned()),
            Expr::Symbol(x) => HashEqExpr::Symbol(*x),
            Expr::Nil => HashEqExpr::Nil,
            _ => {
                return Err(self.make_err(
                    "not-hashable",
                    "expr cannot be hashed".to_string(),
                    Some(x.to_owned()),
                ))
            }
        })
    }

    /// Turned a hashable expression back into a normal one.
    /// Note this is not a reversable transformation; it creates a new unique Gc.
    pub fn from_hashable(&self, x: &HashEqExpr) -> Gc<Expr> {
        Gc::new(match x {
            HashEqExpr::Integer(x) => Expr::Integer(*x),
            HashEqExpr::Float(x) => Expr::Float(*x),
            HashEqExpr::String(x) => Expr::String(x.to_owned()),
            HashEqExpr::Symbol(x) => Expr::Symbol(*x),
            HashEqExpr::Nil => Expr::Nil,
        })
    }
}

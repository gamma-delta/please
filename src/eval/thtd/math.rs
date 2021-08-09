//! Mathematics? That's for eggheads!
//! Also boolean operators.
use crate::{eval::TailRec, Value};

use super::*;

#[derive(Clone, Copy)]
enum Num {
    Int(i64),
    Float(f64),
}

impl Num {
    fn to_expr(self) -> Value {
        Gc::new(match self {
            Num::Int(int) => Expr::Integer(int),
            Num::Float(float) => Expr::Float(float),
        })
    }

    fn from_expr(engine: &mut Engine, expr: Gc<Expr>, idx: usize) -> Result<Num, Exception> {
        Ok(if let Expr::Integer(int) = &*expr {
            Num::Int(*int)
        } else if let Expr::Float(float) = &*expr {
            Num::Float(*float)
        } else {
            return Err(bad_arg_type(engine, expr.clone(), idx, "number"));
        })
    }

    fn as_float(self) -> f64 {
        match self {
            Num::Int(int) => int as _,
            Num::Float(float) => float,
        }
    }
}

macro_rules! num_ops {
    (($trait:path => $name:ident $op:tt)) => {
        impl $trait for Num {
            type Output = Self;
            fn $name(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Num::Int(l), Num::Int(r)) => Num::Int(l $op r),
                    (Num::Float(l), Num::Int(r)) => Num::Float(l $op r as f64),
                    (Num::Int(l), Num::Float(r)) => Num::Float(l as f64 $op r),
                    (Num::Float(l), Num::Float(r)) => Num::Float(l $op r),
                }
            }
        }
    };
    ($head:tt $($tail:tt)*) => {
        num_ops! { $head }
        num_ops! { $($tail)* }
    }
}

num_ops! {
    (std::ops::Add => add +)
    (std::ops::Sub => sub -)
    (std::ops::Mul => mul *)
    (std::ops::Div => div /)
}

pub fn add(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    let mut sum = Num::Int(0);
    for (idx, arg) in args.iter().enumerate() {
        sum = sum + Num::from_expr(engine, arg.to_owned(), idx)?;
    }
    Ok(sum.to_expr())
}

pub fn sub(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;

    let mut difference = Num::from_expr(engine, args[0].to_owned(), 0)?;

    if args.len() == 1 {
        return Ok(match difference {
            Num::Int(int) => Num::Int(-int),
            Num::Float(float) => Num::Float(-float),
        }
        .to_expr());
    }

    for (idx, arg) in args.iter().enumerate().skip(1) {
        difference = difference - Num::from_expr(engine, arg.to_owned(), idx)?;
    }

    Ok(difference.to_expr())
}

pub fn mul(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    let mut product = Num::Int(1);
    for (idx, arg) in args.iter().enumerate() {
        product = product + Num::from_expr(engine, arg.to_owned(), idx)?;
    }
    Ok(product.to_expr())
}

pub fn div(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;

    let mut quotient = Num::from_expr(engine, args[0].to_owned(), 0)?;

    if args.len() == 1 {
        let q = match quotient {
            Num::Int(i) => i as f64,
            Num::Float(f) => f,
        };
        return Ok(Num::Float(q.recip()).to_expr());
    }

    for (idx, arg) in args.iter().enumerate().skip(1) {
        quotient = quotient + Num::from_expr(engine, arg.to_owned(), idx)?;
    }

    Ok(quotient.to_expr())
}

pub fn and(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    if let Some(last) = args.last() {
        for expr in args.iter().take(args.len() - 1) {
            let evaled = engine.eval_inner(env.clone(), expr.to_owned())?;
            if !engine.is_truthy(evaled.to_owned()) {
                // found our falsy, shortcut out
                return Ok(TailRec::Exit(evaled));
            }
        }
        Ok(TailRec::TailRecur(last.to_owned(), env))
    } else {
        Ok(TailRec::Exit(engine.make_bool(true)))
    }
}

pub fn or(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    if let Some(last) = args.last() {
        for expr in args.iter().take(args.len() - 1) {
            let evaled = engine.eval_inner(env.clone(), expr.to_owned())?;
            if engine.is_truthy(evaled.to_owned()) {
                // found our truthy, shortcut out
                return Ok(TailRec::Exit(evaled));
            }
        }
        Ok(TailRec::TailRecur(last.to_owned(), env))
    } else {
        Ok(TailRec::Exit(engine.make_bool(false)))
    }
}

pub fn not(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    Ok(engine.make_bool(!engine.is_truthy(args[0].to_owned())))
}

pub fn xor(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;
    Ok(engine
        .make_bool(engine.is_truthy(args[0].to_owned()) != engine.is_truthy(args[1].to_owned())))
}

macro_rules! comparisons {
    (($name:ident $op:tt)) => {
        pub fn $name(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
            check_argc(engine, args, 2, 2)?;
            let lhs = Num::from_expr(engine, args[0].to_owned(), 0)?;
            let rhs = Num::from_expr(engine, args[1].to_owned(), 1)?;

            let cmp = if let (Num::Int(l), Num::Int(r)) = (lhs, rhs) {
                l $op r
            } else {
                lhs.as_float() $op rhs.as_float()
            };
            Ok(engine.make_bool(cmp))
        }
    };
    ($head:tt $($tail:tt)*) => {
        comparisons! { $head }
        comparisons! { $($tail)* }
    };
}

comparisons! {
    (lt <)
    (gt >)
    (le <=)
    (ge >=)
}

pub fn num_eq(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    // unwrap because we just checked
    let (lhs, rest) = args.split_first().unwrap();
    let lhs = Num::from_expr(engine, lhs.to_owned(), 0)?;
    let mut all_eq = true;
    for (idx, rhs) in rest.iter().enumerate() {
        let rhs = Num::from_expr(engine, rhs.to_owned(), idx + 1)?;

        let cmp = if let (Num::Int(l), Num::Int(r)) = (lhs, rhs) {
            l == r
        } else {
            (lhs.as_float() - rhs.as_float()).abs() < 1e10
        };
        all_eq = cmp;
        if !cmp {
            break;
        }
    }
    Ok(engine.make_bool(all_eq))
}

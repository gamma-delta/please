//! Mathematics? That's for eggheads!
//! Also boolean operators.
use crate::{eval::TailRec, Value};

use super::*;

use paste::paste;

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

    fn to_string(self) -> String {
        match self {
            Num::Int(int) => int.to_string(),
            Num::Float(float) => float.to_string(),
        }
    }
}

macro_rules! num_ops {
    (($trait:path => $name:ident $op:tt)) => {
        impl $trait for Num {
            type Output = Self;
            fn $name(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Num::Int(l), Num::Int(r)) => paste! { Num::Int(l.[< wrapping_ $name >](r)) },
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
    (std::ops::Rem => rem %)
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
        product = product * Num::from_expr(engine, arg.to_owned(), idx)?;
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
        return if q == 0.0 {
            Err(engine.make_err(
                "arithmetic/div-by-zero",
                format!("cannot divide {} by zero", q),
                Some(Gc::new(Expr::Float(q))),
            ))
        } else {
            Ok(Num::Float(q.recip()).to_expr())
        };
    }

    for (idx, arg) in args.iter().enumerate().skip(1) {
        let rhs = Num::from_expr(engine, arg.to_owned(), idx)?;
        if rhs.as_float() == 0.0 {
            return Err(engine.make_err(
                "arithmetic/div-by-zero",
                format!("cannot divide {} by zero", rhs.to_string()),
                Some(rhs.to_expr()),
            ));
        }
        quotient = quotient / rhs;
    }

    Ok(quotient.to_expr())
}

pub fn rem(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    let dividend = Num::from_expr(engine, args[0].to_owned(), 0)?;
    let divisor = Num::from_expr(engine, args[1].to_owned(), 1)?;

    if divisor.as_float() == 0.0 {
        return Err(engine.make_err(
            "arithmetic/div-by-zero",
            format!("cannot divide {} by zero", divisor.to_string()),
            Some(divisor.to_expr()),
        ));
    }

    let rem = dividend % divisor;
    Ok(rem.to_expr())
}

pub fn pow(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    let lhs = Num::from_expr(engine, args[0].to_owned(), 0)?;
    let rhs = Num::from_expr(engine, args[1].to_owned(), 0)?;
    let res = match (lhs, rhs) {
        (Num::Int(l), Num::Int(r)) => {
            if r > 0 {
                Num::Int(l.pow(r as u32))
            } else {
                Num::Float((l as f64).powi(r as i32))
            }
        }
        (Num::Float(l), Num::Int(r)) => Num::Float(l.powi(r as i32)),
        (Num::Int(l), Num::Float(r)) => Num::Float((l as f64).powf(r)),
        (Num::Float(l), Num::Float(r)) => Num::Float(l.powf(r)),
    };
    Ok(res.to_expr())
}

pub fn mod_(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    let dividend = Num::from_expr(engine, args[0].to_owned(), 0)?;
    let divisor = Num::from_expr(engine, args[1].to_owned(), 1)?;

    if divisor.as_float() == 0.0 {
        return Err(engine.make_err(
            "arithmetic/div-by-zero",
            format!("cannot divide {} by zero", divisor.to_string()),
            Some(divisor.to_expr()),
        ));
    }

    let rem = match (dividend, divisor) {
        (Num::Int(l), Num::Int(r)) => Num::Int(l.rem_euclid(r)),
        (Num::Float(l), Num::Int(r)) => Num::Float(l.rem_euclid(r as _)),
        (Num::Int(l), Num::Float(r)) => Num::Float((l as f64).rem_euclid(r)),
        (Num::Float(l), Num::Float(r)) => Num::Float(l.rem_euclid(r)),
    };
    Ok(rem.to_expr())
}

macro_rules! rounders {
    ($name:ident) => {
        pub fn $name(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
            check_argc(engine, args, 1, 1)?;

            let roundee = Num::from_expr(engine, args[0].to_owned(), 0)?;
            Ok(if let Num::Float(f) = roundee {
                Num::Int(f.$name() as i64)
            } else {
                roundee // keep the int
            }.to_expr())
        }
    };
    ($head:tt $($tail:tt)*) => {
        rounders! { $head }
        rounders! { $($tail)* }
    };
}

rounders! {
    round trunc floor ceil
}

pub fn to_inexact(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let it = Num::from_expr(engine, args[0].to_owned(), 0)?;
    Ok(Gc::new(Expr::Float(it.as_float())))
}

macro_rules! bitwise {
    (($name:ident $op:tt)) => {
        pub fn $name(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
            check_argc(engine, args, 2, 2)?;

            let lhs = match &*args[0] {
                Expr::Integer(it) => *it,
                _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "integer")),
            };
            let rhs = match &*args[1] {
                Expr::Integer(it) => *it,
                _ => return Err(bad_arg_type(engine, args[1].clone(), 1, "integer")),
            };
            Ok(Gc::new(Expr::Integer(lhs $op rhs)))
        }
    };
    ($head:tt $($tail:tt)*) => {
        bitwise! { $head }
        bitwise! { $($tail)* }
    };
}

bitwise! {
    (bitwise_and &)
    (bitwise_or |)
    (bitwise_xor ^)
}

pub fn bitwise_not(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let it = match &*args[0] {
        Expr::Integer(it) => *it,
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "integer")),
    };
    Ok(Gc::new(Expr::Integer(!it)))
}

pub fn bitwise_shift(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    let lhs = match &*args[0] {
        Expr::Integer(it) => *it,
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "integer")),
    };
    let rhs = match &*args[1] {
        Expr::Integer(it) => *it,
        _ => return Err(bad_arg_type(engine, args[1].clone(), 1, "integer")),
    };
    let res = if rhs.is_positive() {
        lhs.wrapping_shl(rhs as u32)
    } else {
        lhs.wrapping_shr(-rhs as u32)
    };
    Ok(Gc::new(Expr::Integer(res)))
}

pub fn bitwise_rotate(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let lhs = match &*args[0] {
        Expr::Integer(it) => *it,
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "integer")),
    };
    let rhs = match &*args[1] {
        Expr::Integer(it) => *it,
        _ => return Err(bad_arg_type(engine, args[1].clone(), 1, "integer")),
    };
    let res = if rhs.is_positive() {
        lhs.rotate_left(rhs as u32)
    } else {
        lhs.rotate_right(-rhs as u32)
    };
    Ok(Gc::new(Expr::Integer(res)))
}

pub fn popcnt(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let it = match &*args[0] {
        Expr::Integer(it) => *it,
        _ => return Err(bad_arg_type(engine, args[0].clone(), 0, "integer")),
    };
    Ok(Gc::new(Expr::Integer(it.count_ones() as i64)))
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

//! Mathematics? That's for eggheads!
//! Also boolean operators.
use crate::eval::TailRec;

use super::*;

#[derive(Clone, Copy)]
enum Num {
    Int(i64),
    Float(f64),
}

impl Num {
    fn to_expr(self) -> Gc<Expr> {
        Gc::new(match self {
            Num::Int(int) => Expr::Integer(int),
            Num::Float(float) => Expr::Float(float),
        })
    }

    fn from_expr(engine: &mut Engine, expr: Gc<Expr>, idx: usize) -> Result<Num, Gc<Expr>> {
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

pub fn add(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    let mut sum = Num::Int(0);
    for (idx, arg) in args.iter().enumerate() {
        let num = match Num::from_expr(engine, arg.to_owned(), idx) {
            Ok(it) => it,
            Err(ono) => return ono,
        };
        sum = sum + num;
    }
    sum.to_expr()
}

pub fn sub(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_min_argc(engine, args, 1) {
        return ono;
    };

    let mut difference = match Num::from_expr(engine, args[0].to_owned(), 0) {
        Ok(it) => it,
        Err(ono) => return ono,
    };

    if args.len() == 1 {
        return match difference {
            Num::Int(int) => Num::Int(-int),
            Num::Float(float) => Num::Float(-float),
        }
        .to_expr();
    }

    for (idx, arg) in args.iter().enumerate().skip(1) {
        let num = match Num::from_expr(engine, arg.to_owned(), idx) {
            Ok(it) => it,
            Err(ono) => return ono,
        };
        difference = difference - num;
    }

    difference.to_expr()
}

pub fn mul(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    let mut product = Num::Int(1);
    for (idx, arg) in args.iter().enumerate() {
        let num = match Num::from_expr(engine, arg.to_owned(), idx) {
            Ok(it) => it,
            Err(ono) => return ono,
        };
        product = product * num;
    }
    product.to_expr()
}

pub fn div(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_min_argc(engine, args, 1) {
        return ono;
    };

    let mut quotient = match Num::from_expr(engine, args[0].to_owned(), 0) {
        Ok(it) => it,
        Err(ono) => return ono,
    };

    if args.len() == 1 {
        let q = match quotient {
            Num::Int(i) => i as f64,
            Num::Float(f) => f,
        };
        return Num::Float(q.recip()).to_expr();
    }

    for (idx, arg) in args.iter().enumerate().skip(1) {
        let num = match Num::from_expr(engine, arg.to_owned(), idx) {
            Ok(it) => it,
            Err(ono) => return ono,
        };
        quotient = quotient / num;
    }

    quotient.to_expr()
}

pub fn and(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    let res = args.iter().all(|expr| engine.is_truthy(expr.to_owned()));
    engine.make_bool(res)
}

pub fn or(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    let res = args.iter().any(|expr| engine.is_truthy(expr.to_owned()));
    engine.make_bool(res)
}

pub fn not(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_argc(engine, args, 1, 1) {
        return ono;
    };
    engine.make_bool(!engine.is_truthy(args[0].to_owned()))
}

pub fn xor(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_argc(engine, args, 2, 2) {
        return ono;
    };
    engine.make_bool(engine.is_truthy(args[0].to_owned()) != engine.is_truthy(args[1].to_owned()))
}

pub fn if_(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    if let Err(ono) = check_argc(engine, args, 3, 3) {
        return TailRec::Exit(ono);
    }

    let selector = engine.eval(env.clone(), args[0].to_owned());

    TailRec::TailRecur(
        if engine.is_truthy(selector) {
            args[1].to_owned()
        } else {
            args[2].to_owned()
        },
        env,
    )
}

macro_rules! comparisons {
    (($name:ident $op:tt)) => {
        pub fn $name(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
            if let Err(ono) = check_argc(engine, args, 2, 2) {
                return ono;
            };
            let lhs = match Num::from_expr(engine, args[0].to_owned(), 0) {
                Ok(it) => it,
                Err(ono) => return ono,
            };
            let rhs = match Num::from_expr(engine, args[1].to_owned(), 1) {
                Ok(it) => it,
                Err(ono) => return ono,
            };

            let cmp = if let (Num::Int(l), Num::Int(r)) = (lhs, rhs) {
                l $op r
            } else {
                lhs.as_float() $op rhs.as_float()
            };
            engine.make_bool(cmp)
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

pub fn num_eq(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_min_argc(engine, args, 1) {
        return ono;
    };
    // unwrap because we just checked
    let (lhs, rest) = args.split_first().unwrap();
    let lhs = match Num::from_expr(engine, lhs.to_owned(), 0) {
        Ok(it) => it,
        Err(ono) => return ono,
    };
    let mut all_eq = true;
    for (idx, rhs) in rest.iter().enumerate() {
        let rhs = match Num::from_expr(engine, rhs.to_owned(), idx + 1) {
            Ok(it) => it,
            Err(ono) => return ono,
        };

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
    engine.make_bool(all_eq)
}

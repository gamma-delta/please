//! Mathematics? That's for eggheads!
//! Also boolean operators.
use crate::eval::TailRec;

use super::*;

pub fn add(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    let res: Result<i64, Gc<Expr>> = args
        .iter()
        .enumerate()
        .map(|(idx, arg)| {
            if let Expr::Integer(int) = &**arg {
                Ok(int)
            } else {
                Err(bad_arg_type(engine, arg.clone(), idx, "int"))
            }
        })
        .sum();
    match res {
        Ok(it) => Gc::new(Expr::Integer(it)),
        Err(ono) => ono,
    }
}

pub fn sub(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_min_argc(engine, args, 1) {
        return ono;
    };

    let initial = if let Expr::Integer(num) = &*args[0].to_owned() {
        *num
    } else {
        return bad_arg_type(engine, args[0].to_owned(), 0, "int");
    };
    Gc::new(Expr::Integer(if args.len() == 1 {
        -initial
    } else {
        let rest_sum: Result<i64, _> = args[1..]
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                if let Expr::Integer(int) = **expr {
                    Ok(int)
                } else {
                    Err(bad_arg_type(engine, expr.to_owned(), idx, "int"))
                }
            })
            .sum();
        match rest_sum {
            Ok(it) => initial - it,
            Err(ono) => return ono,
        }
    }))
}

pub fn mul(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    let mut product = 1;
    for (idx, arg) in args.iter().enumerate() {
        if let Expr::Integer(int) = **arg {
            product *= int
        } else {
            return bad_arg_type(engine, arg.clone(), idx, "int");
        }
    }
    Gc::new(Expr::Integer(product))
}

pub fn div_floor(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_min_argc(engine, args, 2) {
        return ono;
    };

    let mut quotient = if let Expr::Integer(num) = &*args[0].to_owned() {
        *num
    } else {
        return bad_arg_type(engine, args[0].to_owned(), 0, "int");
    };

    for (idx, expr) in args[1..].iter().enumerate() {
        if let Expr::Integer(int) = **expr {
            quotient /= int;
        } else {
            return bad_arg_type(engine, expr.to_owned(), idx, "int");
        }
    }

    Gc::new(Expr::Integer(quotient))
}

pub fn and(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    let res = args.iter().all(|expr| engine.is_truthy(expr.to_owned()));
    engine.make_bool(res)
}

pub fn or(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    let res = args.iter().any(|expr| engine.is_truthy(expr.to_owned()));
    engine.make_bool(res)
}

pub fn not(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_argc(engine, args, 1, 1) {
        return ono;
    };
    engine.make_bool(!engine.is_truthy(args[0].to_owned()))
}

pub fn xor(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
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

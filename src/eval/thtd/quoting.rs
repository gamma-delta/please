//! Quotes, unquotes, and quasiquotes.
use super::*;
use crate::eval::TailRec;

/// Note this is the "function" quote which should just return its argument.
pub fn quote(engine: &mut Engine, _env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    TailRec::Exit(if let Err(e) = check_argc(engine, args, 1, 1) {
        e
    } else {
        args[0].to_owned()
    })
}

pub fn quasiquote(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    if let Err(e) = check_argc(engine, args, 1, 1) {
        return TailRec::Exit(e);
    }

    let arg = args[0].to_owned();
    // someone smarter than i could make this tail rec
    TailRec::Exit(quasi_helper(engine, env, arg))
}

fn quasi_helper(engine: &mut Engine, env: Gc<GcCell<Namespace>>, arg: Gc<Expr>) -> Gc<Expr> {
    let unquote = engine.find_symbol("unquote").unwrap();
    match &*arg {
        Expr::Pair(car, cdr) => {
            if matches!(&**car, Expr::Symbol(sym) if *sym == unquote) {
                let actual_cdr = match &**cdr {
                    Expr::Pair(car, cdr) => {
                        if let Expr::Nil = &**cdr {
                            car.to_owned()
                        } else {
                            return bad_arg_type(engine, cdr.to_owned(), 1, "1-list");
                        }
                    }
                    _ => return bad_arg_type(engine, cdr.to_owned(), 1, "1-list"),
                };
                engine.eval(env, actual_cdr)
            } else {
                let car = quasi_helper(engine, env.clone(), car.to_owned());
                let cdr = quasi_helper(engine, env, cdr.to_owned());
                Gc::new(Expr::Pair(car, cdr))
            }
        }
        _ => arg,
    }
}

pub fn unquote(engine: &mut Engine, _env: Gc<GcCell<Namespace>>, _args: &[Gc<Expr>]) -> TailRec {
    TailRec::Exit(engine.make_err(
        "cannot directly call unquote (must use within quasiquote)".to_string(),
        None,
    ))
}

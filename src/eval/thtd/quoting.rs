//! Quotes, unquotes, and quasiquotes.
use super::*;
use crate::eval::TailRec;

/// Note this is the "function" quote which should just return its argument.
pub fn quote(
    engine: &mut Engine,
    _env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 1, 1)?;
    Ok(TailRec::Exit(args[0].to_owned()))
}

pub fn quasiquote(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 1, 1)?;

    let arg = args[0].to_owned();
    Ok(TailRec::Exit(quasi_helper(engine, env, arg, None)?))
}

fn quasi_helper(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    arg: Gc<Expr>,
    pcdr: Option<Gc<Expr>>,
) -> EvalResult {
    let unquote = engine.find_symbol("unquote").unwrap();
    let unquote_splice = engine.find_symbol("unquote-splicing").unwrap();
    let arg = match &*arg {
        Expr::Pair(..) | Expr::LazyPair(..) => {
            let (car, cdr) = engine.split_cons(arg)?;
            match &*car {
                Expr::Symbol(sym) if *sym == unquote => {
                    let actual_cdr = match &*cdr {
                        Expr::Pair(..) | Expr::LazyPair(..) => {
                            let (car, cdr) = engine.split_cons(cdr.clone())?;
                            if let Expr::Nil = &*cdr {
                                car
                            } else {
                                return Err(bad_arg_type(engine, cdr, 1, "1-list"));
                            }
                        }
                        _ => return Err(bad_arg_type(engine, cdr, 1, "1-list")),
                    };
                    engine.eval_inner(env.clone(), actual_cdr)?
                }
                Expr::Symbol(sym) if *sym == unquote_splice => return match pcdr {
                    Some(exp) => {
                        let actual_cdr = match &*cdr {
                            Expr::Pair(..) | Expr::LazyPair(..) => {
                                let (car, cdr) = engine.split_cons(cdr.clone())?;
                                if let Expr::Nil = &*cdr {
                                    car
                                } else {
                                    return Err(bad_arg_type(engine, cdr, 1, "1-list"));
                                }
                            }
                            _ => return Err(bad_arg_type(engine, cdr, 1, "1-list")),
                        };
                        let sexp = engine.eval_inner(env.clone(), actual_cdr.clone())?;
                        let list = engine
                            .sexp_to_list(sexp)?
                            .ok_or_else(|| bad_arg_type(engine, actual_cdr, 1, "list"))?;
                        let last = quasi_helper(engine, env, exp, None)?;
                        Ok(Engine::list_to_improper_sexp(&list[..], last))
                    }
                    None => Err(engine.make_err(
                        "quote/bad-splice",
                        "used unquote-splice in illegal position".to_string(),
                        None,
                    )),
                },
                _ => {
                    quasi_helper(engine, env.clone(), car, Some(cdr))?
                }
            }
        }
        _ => arg,
    };

    Ok(match pcdr.clone() {
        Some(cdr) => Gc::new(Expr::Pair(arg, quasi_helper(engine, env.clone(), cdr, None)?)),
        None => arg,
    })
}

pub fn unquote(
    engine: &mut Engine,
    _env: Gc<GcCell<Namespace>>,
    _args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    Err(engine.make_err(
        "quote/direct-unquote",
        "cannot directly call unquote (must use within quasiquote)".to_string(),
        None,
    ))
}

pub fn unquote_splicing(
    engine: &mut Engine,
    _env: Gc<GcCell<Namespace>>,
    _args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    Err(engine.make_err(
        "quote/direct-unquote",
        "cannot directly call unquote-splicing (must use within quasiquote)".to_string(),
        None,
    ))
}

pub fn eval(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_min_argc(engine, args, 1)?;
    args.iter()
        .map(|expr| engine.eval_inner(env.clone(), expr.to_owned()))
        .last()
        .unwrap()
}

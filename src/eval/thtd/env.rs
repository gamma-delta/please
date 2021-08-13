//! Messing with the environment and namespaces.

use super::*;
use crate::eval::TailRec;
use crate::Expr;

pub fn define(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    define_internals(engine, env, args, true)
}

pub fn define_macro(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    define_internals(engine, env, args, false)
}

fn define_internals(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
    is_lambda: bool,
) -> Result<TailRec, Exception> {
    check_min_argc(engine, args, 2)?;

    let first = args[0].to_owned();
    if let Expr::Symbol(id) = &*first {
        check_argc(engine, args, 2, 2)?;

        let evaled = engine.eval_inner(env.to_owned(), args[1].to_owned())?;
        let evaled = match &*evaled {
            Expr::Procedure {
                args,
                body,
                env,
                variadic,
                name: _old_name,
            } => Gc::new(Expr::Procedure {
                args: args.to_owned(),
                body: body.to_owned(),
                env: env.to_owned(),
                variadic: *variadic,
                name: Some(*id),
            }),
            _ => evaled,
        };
        env.borrow_mut().insert(*id, evaled);
        Ok(TailRec::Exit(Gc::new(Expr::Nil)))
    } else if let Expr::Pair(name, tail) = &*first {
        // > (define (NAME . TAIL) BODY*) becomes (define NAME (lambda TAIL BODY*))
        // --alwinfy
        // construct a sex expression and just have the eval do it for us
        let lambda_name = engine.intern_symbol(if is_lambda { "lambda" } else { "macro" });
        let mut lambda_list = vec![Gc::new(Expr::Symbol(lambda_name)), tail.to_owned()];
        lambda_list.extend_from_slice(&args[1..]);
        let lambda_sexpr = Engine::list_to_sexp(&lambda_list);

        if !matches!(&**name, Expr::Symbol(..)) {
            return Err(bad_arg_type(engine, first, 0, "(symbol, any)"));
        }

        // Make the evaluator do the work for us.
        // Rearrange the arguments and re-eval them; eventually it will settle
        // into the top branch.
        let define_name = engine.intern_symbol("define");
        let define_list = &[
            Gc::new(Expr::Symbol(define_name)),
            name.to_owned(),
            lambda_sexpr,
        ];
        Ok(TailRec::TailRecur(Engine::list_to_sexp(define_list), env))
    } else {
        Err(bad_arg_type(engine, args[0].clone(), 0, "symbol"))
    }
}

pub fn let_(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    mut args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    let inner_env = Gc::new(GcCell::new(Namespace::new(env.clone())));

    let symbol = match args.get(0).map(|s| &**s) {
        Some(Expr::Symbol(s)) => {
            args = &args[1..];
            Some(*s)
        }
        _ => None,
    };

    check_min_argc(engine, args, 2)?;

    let arg_bindings = match engine.sexp_to_list(args[0].to_owned())? {
        Some(it) => it,
        None => {
            return Err(bad_arg_type(
                engine,
                args[0].to_owned(),
                0,
                "list of (symbol expr)",
            ))
        }
    };

    let mut names = vec![];
    let mut evaluated = vec![];
    for binding in arg_bindings {
        if let Some(pair) = engine.sexp_to_list(binding)? {
            if let [name, expr] = pair.as_slice() {
                if let Expr::Symbol(id) = **name {
                    let evaled = engine.eval_inner(inner_env.clone(), expr.to_owned())?;
                    inner_env.borrow_mut().insert(id, evaled.clone());
                    names.push((id, None));
                    evaluated.push(evaled);
                    continue;
                }
            }
        }
        return Err(bad_arg_type(
            engine,
            args[0].to_owned(),
            0,
            "list of (symbol expr)",
        ));
    }

    // Now eval the bodies
    // skip the last for tail positioning
    if let Some(s) = symbol {
        let lambda = Gc::new(Expr::Procedure {
            args: names,
            body: args[1..].to_vec(),
            env: Some(inner_env.clone()),
            variadic: false,
            name: Some(s),
        });
        inner_env.borrow_mut().insert(s, lambda.clone());
    }

    for body in &args[1..args.len() - 1] {
        engine.eval_inner(inner_env.clone(), body.to_owned())?;
    }
    Ok(TailRec::TailRecur(
        args.last().unwrap().to_owned(),
        inner_env,
    ))
}

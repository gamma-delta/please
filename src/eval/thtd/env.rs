//! Messing with the environment and namespaces.

use super::*;
use crate::eval::TailRec;
use crate::Expr;

pub fn define(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 2, 2)?;

    let spec = args[0].to_owned();
    let val = engine.eval_inner(env.clone(), args[1].to_owned())?;
    let bindings = engine.destructure_assign(env.to_owned(), spec, val.to_owned())?;
    env.borrow_mut().merge_from(bindings);

    Ok(TailRec::Exit(val))
}

pub fn let_(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    mut args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_min_argc(engine, args, 2)?;

    let loop_form = match args.get(0).map(|expr| &**expr) {
        Some(Expr::Symbol(sym)) => {
            args = &args[1..];
            Some(*sym)
        }
        _ => None,
    };

    let arg_bindings = match engine.sexp_to_list(args[0].to_owned())? {
        Some(it) => it,
        None => {
            return Err(bad_arg_type(
                engine,
                args[0].to_owned(),
                if loop_form.is_some() { 1 } else { 0 },
                "list of (spec expr)",
            ))
        }
    };

    let inner_env = Gc::new(GcCell::new(Namespace::new(env)));
    let mut specs = Vec::with_capacity(arg_bindings.len());
    for binding in arg_bindings {
        if let Some(pair) = engine.sexp_to_list(binding)? {
            if let [spec, expr] = pair.as_slice() {
                let evaled = engine.eval_inner(inner_env.clone(), expr.to_owned())?;
                let bindings =
                    engine.destructure_assign(inner_env.to_owned(), spec.to_owned(), evaled)?;
                inner_env.borrow_mut().merge_from(bindings);
                specs.push(spec.to_owned());
                continue;
            }
        }
        return Err(bad_arg_type(
            engine,
            args[0].to_owned(),
            0,
            "list of (spec expr)",
        ));
    }

    // Now eval the bodies
    // skip the last for tail positioning
    if let Some(s) = loop_form {
        let arg_spec = Engine::list_to_sexp(&specs);
        let lambda = Gc::new(Expr::Procedure {
            arg_spec,
            body: args[1..].to_vec(),
            env: Some(inner_env.clone()),
            name: Some(s),
        });
        inner_env.borrow_mut().insert(s, lambda);
    }

    for body in &args[1..args.len() - 1] {
        engine.eval_inner(inner_env.clone(), body.to_owned())?;
    }
    Ok(TailRec::TailRecur(
        args.last().unwrap().to_owned(),
        inner_env,
    ))
}

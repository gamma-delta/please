use std::time::{Duration, Instant};

use crate::{Engine, EvalResult, Exception, Expr, Namespace, Symbol, Value};

mod destructure;
pub mod thtd;
use gc::{Gc, GcCell};
pub use thtd::{add_thtandard_library, bad_arg_type};

/// Do we use tail recursion for a special form?
pub enum TailRec {
    /// No, return this and exit
    Exit(Value),
    /// Yes, eval this in the given namespace
    TailRecur(Value, Gc<GcCell<Namespace>>),
}

impl Engine {
    pub fn eval(&mut self, env: Gc<GcCell<Namespace>>, expr: Value) -> Value {
        let result = self.eval_inner(env, expr);
        result.unwrap_or_else(|exn| exn.into_expr(self))
    }

    /// Evaluate the expression at the given location on the heap,
    /// put the result on the heap, and return it.
    pub fn eval_inner(&mut self, mut env: Gc<GcCell<Namespace>>, mut expr: Gc<Expr>) -> EvalResult {
        loop {
            match self.eval_rec(env.clone(), expr) {
                Ok(TailRec::Exit(val)) => break Ok(val),
                Err(ono) => break Err(ono),
                Ok(TailRec::TailRecur(next, nenv)) => {
                    expr = next;
                    env = nenv;
                }
            };
        }
    }
    /// Helper function that either returns Err(next expr) or Ok(final result).
    fn eval_rec(
        &mut self,
        env: Gc<GcCell<Namespace>>,
        expr: Gc<Expr>,
    ) -> Result<TailRec, Exception> {
        match &*expr {
            // Passthru literals unchanged
            Expr::Integer(_)
            | Expr::Float(_)
            | Expr::Nil
            | Expr::String(_)
            | Expr::SpecialForm { .. }
            | Expr::NativeProcedure { .. }
            | Expr::Procedure { .. }
            | Expr::Map(_) => Ok(TailRec::Exit(expr)),
            // Lookup the symbol
            &Expr::Symbol(id) => {
                let idx = env.borrow().lookup(id);
                let msg = self.write_expr(expr.clone())?;
                match idx {
                    Some(it) => Ok(TailRec::Exit(it)),
                    None => Err(self.make_err(
                        "undefined",
                        format!("'{} is undefined", msg),
                        Some(expr),
                    )),
                }
            }
            Expr::Pair(..) | Expr::LazyPair(..) => {
                let (car, cdr) = self.split_cons(expr.clone())?;
                let func = self.eval_inner(env.clone(), car)?;

                let mut args = match self.sexp_to_list(cdr.clone())? {
                    Some(it) => it,
                    None => {
                        return Err(self.make_err(
                            "application/cdr-list",
                            "application: cdr must be a proper list".to_string(),
                            Some(cdr),
                        ))
                    }
                };

                // Specially handle macros.
                match &*func {
                    Expr::SpecialForm { func, name } => {
                        return func(self, env, &args).map_err(|mut exn| {
                            exn.call_trace.trace.push(Some(*name));
                            exn
                        })
                    }
                    Expr::Procedure {
                        env: None,
                        arg_spec,
                        body,
                        name,
                    } => {
                        return self
                            .call_procedure(
                                env,
                                args,
                                arg_spec.to_owned(),
                                body.to_owned(),
                                None,
                                *name,
                            )
                            .map(|(tr, _)| tr)
                            .map_err(|mut exn| {
                                exn.call_trace.trace.push(*name);
                                exn
                            })
                    }
                    _ => {}
                }

                // Ok, onto application
                // So we don't try to squish the last argument in, push a nil.
                args.push(Expr::nil());

                let evaled_args = args
                    .into_iter()
                    .map(|expr| self.eval_inner(env.clone(), expr))
                    .collect::<Result<Vec<_>, _>>()?;

                self.apply_inner(env, func, evaled_args)
            }
        }
    }

    /// Convert (fn a1 a2 (trail)) into (fn a1 a2 ...trail), evaluate it, and return the result.
    /// Each of the arguments should be pre-evaluated.
    ///
    /// To avoid treating the last argument specially, just push a `nil` onto the end of of the arguments.
    fn apply_inner(
        &mut self,
        env: Gc<GcCell<Namespace>>,
        func: Gc<Expr>,
        mut args: Vec<Gc<Expr>>,
    ) -> Result<TailRec, Exception> {
        if let Some(trail) = args.pop() {
            match self.sexp_to_list(trail.to_owned())? {
                Some(trail) => args.extend(trail),
                None => {
                    return Err(self.make_err(
                        "application/trail-non-list",
                        "last argument was not a list".to_string(),
                        Some(trail),
                    ))
                }
            }
        } // else it's a niladic function, I guess? kinda sus.

        let out = match &*func {
            &Expr::NativeProcedure { func, name } => {
                let now = Instant::now();
                let out = match func {
                    Ok(func) => func(self, env, &args).map(TailRec::Exit),
                    Err(tailfunc) => tailfunc(self, env, &args),
                };
                out.map(|tr| (tr, now.elapsed(), name)).map_err(|mut e| {
                    e.call_trace.trace.push(Some(name));
                    e
                })
            }
            Expr::Procedure {
                arg_spec,
                body,
                env: Some(closed_env),
                name,
            } => {
                let res = self.call_procedure(
                    env,
                    args,
                    arg_spec.to_owned(),
                    body.to_owned(),
                    Some(closed_env.to_owned()),
                    *name,
                );

                res.map(|(tr, dt)| {
                    (
                        tr,
                        dt,
                        name.unwrap_or_else(|| self.intern_symbol("<anonymous>")),
                    )
                })
                .map_err(|mut e| {
                    e.call_trace.trace.push(*name);
                    e
                })
            }
            Expr::SpecialForm { .. } | Expr::Procedure { env: None, .. } => Err(self.make_err(
                "application/macro",
                "cannot apply a macro".to_string(),
                Some(func),
            )),
            _ => Err(self.make_err(
                "application/not-callable",
                "not callable".to_string(),
                Some(func),
            )),
        };

        out.map(|(tr, dt, name)| {
            if let Some(profiler) = self.profiler.as_mut() {
                let dt = dt.as_secs_f64();
                match profiler.get_mut(&name) {
                    Some((count, total_time)) => {
                        *count += 1;
                        *total_time += dt;
                    }
                    None => {
                        profiler.insert(name, (1, dt));
                    }
                }
            }
            tr
        })
    }

    /// Call a user-defined procedure.
    ///
    /// This does not eval the passed arguments, so if this is a lambda, pre-eval them before passing them.
    #[allow(clippy::too_many_arguments)]
    fn call_procedure(
        &mut self,
        env: Gc<GcCell<Namespace>>,
        args_passed: Vec<Gc<Expr>>,
        arg_spec: Gc<Expr>,
        body: Vec<Gc<Expr>>,
        closed_env: Option<Gc<GcCell<Namespace>>>,
        _name: Option<Symbol>,
    ) -> Result<(TailRec, Duration), Exception> {
        let args_passed = Engine::list_to_sexp(&args_passed);
        let assigned_args = self.destructure_assign(arg_spec, args_passed)?;

        // disposable environment filled with arguments
        let arg_env = Gc::new(GcCell::new(match &closed_env {
            // lambdas are called closing over their environment
            Some(closed) => Namespace::new_with(closed.to_owned(), assigned_args),
            // macros are just executed in the parent context
            None => Namespace::new_with(env.to_owned(), assigned_args),
        }));

        let (body, tail) = match &body[..] {
            [body @ .., tail] => (body, tail),
            [] => {
                return Err(self.make_err(
                    "application/no-body",
                    "application: had a procedure with no body sexprs".to_string(),
                    None,
                ));
            }
        };

        let now = Instant::now();
        for expr in body {
            self.eval_inner(arg_env.clone(), expr.clone())?;
        }
        let tr = if closed_env.is_some() {
            // Tail recurse on the tail
            TailRec::TailRecur(tail.to_owned(), arg_env)
        } else {
            // Macros are double-evaluated, and the second time is *in the parent context*
            // this way things like define work
            let evaled_once = self.eval_inner(arg_env, tail.to_owned())?;
            TailRec::TailRecur(evaled_once, env)
        };
        Ok((tr, now.elapsed()))
    }
}

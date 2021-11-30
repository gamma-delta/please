use crate::{Engine, EvalResult, Exception, Expr, Namespace, Value};

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
                        "application/undefined",
                        format!("application: '{} is undefined", msg),
                        Some(expr),
                    )),
                }
            }
            Expr::Pair(..) | Expr::LazyPair(..) => {
                let (car, cdr) = self.split_cons(expr.clone())?;
                let car = self.eval_inner(env.clone(), car)?;

                let args = match self.sexp_to_list(cdr.clone())? {
                    Some(it) => it,
                    None => {
                        return Err(self.make_err(
                            "application/cdr-list",
                            "application: cdr must be a proper list".to_string(),
                            Some(cdr),
                        ))
                    }
                };
                match &*car {
                    &Expr::SpecialForm { func, name } => func(self, env, &args).map_err(|mut e| {
                        e.call_trace.trace.push(Some(name));
                        e
                    }),
                    &Expr::NativeProcedure { func, name } => {
                        let trynt = || {
                            let evaled_args = args
                                .into_iter()
                                .map(|expr| self.eval_inner(env.clone(), expr))
                                .collect::<Result<Vec<_>, _>>()?;

                            match func {
                                Ok(func) => func(self, env, &evaled_args).map(TailRec::Exit),
                                Err(tailfunc) => tailfunc(self, env, &evaled_args),
                            }
                        };
                        let result = trynt();
                        result.map_err(|mut e| {
                            e.call_trace.trace.push(Some(name));
                            e
                        })
                    }
                    Expr::Procedure {
                        args: arg_names,
                        body,
                        env: closed_env,
                        variadic,
                        name,
                    } => {
                        let trynt = || {
                            // Fill the arg slots via a new namespace
                            let mut arg_env = Namespace::new(
                                closed_env.as_ref().cloned().unwrap_or_else(|| env.clone()),
                            );

                            // Eval the args in the parent context
                            let args_passed = if closed_env.is_some() {
                                args
                                    // eval args for a function call
                                    .into_iter()
                                    .map(|arg| self.eval_inner(env.clone(), arg))
                                    .collect::<Result<Vec<_>, _>>()?
                            } else {
                                // let them be for a macro
                                args
                            };

                            let minimum_argc = arg_names
                                .iter()
                                .filter(|(_, default)| default.is_none())
                                .count();
                            if *variadic {
                                thtd::check_min_argc(self, &args_passed, minimum_argc - 1)?;
                            } else {
                                thtd::check_argc(
                                    self,
                                    &args_passed,
                                    minimum_argc,
                                    arg_names.len(),
                                )?;
                            }

                            let (arg_names, variadic) = if *variadic {
                                (
                                    &arg_names[..arg_names.len() - 1],
                                    Some(arg_names.last().unwrap().0),
                                )
                            } else {
                                (&arg_names[..], None)
                            };

                            let mut args = &args_passed[..];
                            for (name, default) in arg_names {
                                let val = match (args, default) {
                                    // Fill next arg
                                    ([first, rest @ ..], _) => {
                                        args = rest;
                                        first
                                    }
                                    // Use a def'n'ed default
                                    ([], Some(v)) => v,
                                    // We passed too many arguments or too few
                                    // should have already checked for this
                                    _ => unreachable!(),
                                };
                                arg_env.insert(*name, val.clone());
                            }
                            if let Some(name) = variadic {
                                let trail = Engine::list_to_sexp(args);
                                arg_env.insert(name, trail);
                            }

                            let arg_env = Gc::new(GcCell::new(arg_env));
                            let body_env = if closed_env.is_some() {
                                // lambdas are called closing over their environment
                                arg_env.clone()
                            } else {
                                // macros just use the parent environment with args,
                                // so create a disposable env for it to mess up
                                Gc::new(GcCell::new(Namespace::new(arg_env.clone())))
                            };

                            let (body, tail) = match &body[..] {
                                [body @ .., tail] => (body, tail),
                                [] => {
                                    return Err(self.make_err(
                                        "application/no-body",
                                        "application: had a procedure with no body sexprs"
                                            .to_string(),
                                        None,
                                    ));
                                }
                            };
                            for expr in body {
                                self.eval_inner(body_env.clone(), expr.clone())?;
                            }
                            let last = if closed_env.is_some() {
                                tail.to_owned()
                            } else {
                                self.eval_inner(arg_env.clone(), tail.clone())?
                            };
                            let last_env = if closed_env.is_some() { arg_env } else { env };
                            Ok(TailRec::TailRecur(last, last_env))
                        };
                        let res = trynt();
                        res.map_err(|mut e| {
                            e.call_trace.trace.push(*name);
                            e
                        })
                    }
                    _ => Err(self.make_err(
                        "application/not-callable",
                        "application: not callable".to_string(),
                        Some(car),
                    )),
                }
            }
        }
    }
}

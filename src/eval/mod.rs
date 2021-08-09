use crate::{Engine, EvalResult, Exception, Expr, Namespace, Value};

mod thtd;
use gc::{Gc, GcCell};
use itertools::{EitherOrBoth, Itertools};
pub use thtd::add_thtandard_library;

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
    fn eval_inner(&mut self, mut env: Gc<GcCell<Namespace>>, mut expr: Gc<Expr>) -> EvalResult {
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
    /// Helper function that either returns Err(next expr) or Ok(final result)
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
            | Expr::Procedure { .. } => Ok(TailRec::Exit(expr)),
            // Lookup the symbol
            &Expr::Symbol(id) => {
                let idx = env.borrow().lookup(id);
                match idx {
                    Some(it) => Ok(TailRec::Exit(it)),
                    None => Err(self.make_err(
                        "application/undefined",
                        format!(
                            "application: '{} is undefined",
                            self.write_expr(expr.clone())
                        ),
                        Some(expr),
                    )),
                }
            }
            Expr::Pair(car, cdr) => {
                let car = self.eval_inner(env.clone(), car.clone())?;

                let args = match Engine::sexp_to_list(cdr.clone()) {
                    Some(it) => it,
                    None => {
                        return Err(self.make_err(
                            "application/cdr-list",
                            "application: cdr must be a proper list".to_string(),
                            Some(cdr.clone()),
                        ))
                    }
                };
                match &*car {
                    &Expr::SpecialForm { func, .. } => func(self, env, &args),
                    &Expr::NativeProcedure { func, .. } => {
                        let evaled_args = args
                            .into_iter()
                            .map(|expr| self.eval_inner(env.clone(), expr))
                            .collect::<Result<Vec<_>, _>>()?;

                        func(self, env, &evaled_args).map(TailRec::Exit)
                    }
                    Expr::Procedure {
                        args: arg_names,
                        body,
                        env: closed_env,
                        variadic,
                    } => {
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
                            thtd::check_argc(self, &args_passed, minimum_argc, args_passed.len())?;
                        }

                        for (idx, arg_default) in
                            arg_names.iter().zip_longest(&args_passed).enumerate()
                        {
                            let (symbol, val) = match arg_default {
                                // We have a value to fill in the rhs, ignore default
                                EitherOrBoth::Both((sym, _default), val) => (*sym, val.to_owned()),
                                // We don't have any more args passed, but we do have a default
                                EitherOrBoth::Left((sym, Some(default))) => {
                                    (*sym, default.to_owned())
                                }
                                // We passed too many arguments or too few
                                // should have already checked for this
                                _ => unreachable!(),
                            };
                            if *variadic && idx == arg_names.len() - 1 {
                                // This is the trail arg
                                let trail = Engine::list_to_sexp(&args_passed[idx..]);
                                arg_env.insert(symbol, trail);
                                break; // Break to prevent the next iteration from "running out" of args
                            } else {
                                arg_env.insert(symbol, val);
                            }
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
                                    "application: had a procedure with no body sexprs".to_string(),
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
                        Ok(TailRec::TailRecur(last, arg_env))
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

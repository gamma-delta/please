use crate::{Engine, Expr, Namespace};

mod thtd;
use gc::{Gc, GcCell};
use itertools::{EitherOrBoth, Itertools};
pub use thtd::add_thtandard_library;

/// Do we use tail recursion for a special form?
pub enum TailRec {
    /// No, return this and exit
    Exit(Gc<Expr>),
    /// Yes, eval this in the given namespace
    TailRecur(Gc<Expr>, Gc<GcCell<Namespace>>),
}

impl Engine {
    /// Evaluate the expression at the given location on the heap,
    /// put the result on the heap, and return it.
    pub fn eval(&mut self, mut env: Gc<GcCell<Namespace>>, mut expr: Gc<Expr>) -> Gc<Expr> {
        loop {
            match self.eval_rec(env.clone(), expr) {
                TailRec::Exit(val) => break val,
                TailRec::TailRecur(next, nenv) => {
                    expr = next;
                    env = nenv;
                }
            };
        }
    }
    /// Helper function that either returns Err(next expr) or Ok(final result)
    fn eval_rec(&mut self, env: Gc<GcCell<Namespace>>, expr: Gc<Expr>) -> TailRec {
        match &*expr {
            // Passthru literals unchanged
            Expr::Integer(_)
            | Expr::Float(_)
            | Expr::Nil
            | Expr::String(_)
            | Expr::SpecialForm { .. }
            | Expr::NativeProcedure { .. }
            | Expr::Procedure { .. } => TailRec::Exit(expr),
            // Lookup the symbol
            &Expr::Symbol(id) => {
                let idx = env.borrow().lookup(id);
                let res = match idx {
                    Some(it) => it,
                    None => self.make_err(
                        format!(
                            "application: '{} is undefined",
                            self.write_expr(expr.clone())
                        ),
                        Some(expr),
                    ),
                };
                TailRec::Exit(res)
            }
            Expr::Pair(car, cdr) => {
                let car = self.eval(env.clone(), car.clone());

                let args = match self.sexp_to_list(cdr.clone()) {
                    Some(it) => it,
                    None => {
                        return TailRec::Exit(self.make_err(
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
                            .map(|expr| self.eval(env.clone(), expr))
                            .collect::<Vec<_>>();

                        TailRec::Exit(func(self, env, &evaled_args))
                    }
                    Expr::Procedure {
                        args: arg_names,
                        body,
                        env: closed_env,
                        variadic,
                        is_lambda,
                    } => {
                        // Fill the arg slots via a new namespace
                        let mut arg_env = Namespace::new(closed_env.clone());

                        let args_passed = if *is_lambda {
                            // eval args in parent context for a function call
                            args.into_iter()
                                .map(|arg| self.eval(env.clone(), arg))
                                .collect::<Vec<_>>()
                        } else {
                            // let them be for a macro
                            args
                        };

                        let minimum_argc = arg_names
                            .iter()
                            .filter(|(_, default)| default.is_none())
                            .count();
                        let argc_checker = if *variadic {
                            thtd::check_min_argc(self, &args_passed, minimum_argc - 1)
                        } else {
                            thtd::check_argc(self, &args_passed, minimum_argc, args_passed.len())
                        };
                        if let Err(ono) = argc_checker {
                            return TailRec::Exit(ono);
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
                                let trail = self.list_to_sexp(&args_passed[idx..]);
                                arg_env.insert(symbol, trail);
                                break; // Break to prevent the next iteration from "running out" of args
                            } else {
                                arg_env.insert(symbol, val);
                            }
                        }

                        let arg_env = if *is_lambda {
                            // lambdas are called closing over their environment
                            Gc::new(GcCell::new(arg_env))
                        } else {
                            // macros just use the parent environment
                            env
                        };

                        let (body, tail) = match &body[..] {
                            [body @ .., tail] => (body, tail),
                            [] => {
                                return TailRec::Exit(self.make_err(
                                    "application: had a procedure with no body sexprs".to_string(),
                                    None,
                                ))
                            }
                        };
                        for expr in body {
                            self.eval(arg_env.clone(), expr.clone());
                        }
                        let last = if *is_lambda {
                            tail.to_owned()
                        } else {
                            self.eval(arg_env.clone(), tail.clone())
                        };
                        TailRec::TailRecur(last, arg_env)
                    }
                    _ => TailRec::Exit(
                        self.make_err("application: not a procedure".to_string(), Some(car)),
                    ),
                }
            }
        }
    }
}

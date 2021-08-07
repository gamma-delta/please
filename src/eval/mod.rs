use crate::{Engine, Expr, Namespace};

mod thtd;
use gc::{Gc, GcCell};
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
                    } => {
                        // Fill the arg slots via a new namespace
                        let mut arg_env = Namespace::new(closed_env.clone());

                        // Eval the args in the parent context
                        let args_passed_evaled = args
                            .into_iter()
                            .map(|arg| self.eval(env.clone(), arg))
                            .collect::<Vec<_>>();

                        for (idx, &symbol) in arg_names.iter().enumerate() {
                            if *variadic && idx == arg_names.len() - 1 {
                                // This is the trail arg
                                let trail = self.list_to_sexp(&args_passed_evaled[idx..]);
                                arg_env.insert(symbol, trail);
                            } else if let Some(arg) = args_passed_evaled.get(idx) {
                                arg_env.insert(symbol, arg.clone());
                            } else {
                                // Uh oh we ran out of args in the call
                                let message = format!(
                                    "application: expected {}{} args but only got {}",
                                    arg_names.len(),
                                    if *variadic { " or more" } else { "" },
                                    idx
                                );
                                let truth = if *variadic {
                                    self.intern_symbol("true")
                                } else {
                                    self.intern_symbol("false")
                                };
                                let data = self.list_to_sexp(&[
                                    Gc::new(Expr::Integer(arg_names.len() as _)),
                                    Gc::new(Expr::Symbol(truth)),
                                    Gc::new(Expr::Integer(idx as _)),
                                ]);
                                return TailRec::Exit(self.make_err(message, Some(data)));
                            }
                        }

                        let arg_env = Gc::new(GcCell::new(arg_env));

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
                        TailRec::TailRecur(tail.clone(), arg_env)
                    }
                    _ => TailRec::Exit(
                        self.make_err("application: not a procedure".to_string(), Some(car)),
                    ),
                }
            }
        }
    }
}

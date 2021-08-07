use crate::{Engine, Expr, Namespace};

mod thtd;
use gc::{Gc, GcCell};
pub use thtd::add_thtandard_library;

impl Engine {
    /// Evaluate the expression at the given location on the heap,
    /// put the result on the heap, and return it.
    pub fn eval(&mut self, env: Gc<GcCell<Namespace>>, expr: Gc<Expr>) -> Gc<Expr> {
        match &*expr {
            // Passthru literals unchanged
            Expr::Integer(_)
            | Expr::Nil
            | Expr::String(_)
            | Expr::SpecialForm { .. }
            | Expr::NativeProcedure { .. }
            | Expr::Procedure { .. } => expr,
            // Lookup the symbol
            &Expr::Symbol(id) => {
                let idx = env.borrow().lookup(id);
                match idx {
                    Some(it) => it,
                    None => self.make_err(
                        format!(
                            "application: '{} is undefined",
                            self.write_expr(expr.clone())
                        ),
                        Some(expr),
                    ),
                }
            }
            Expr::Pair(car, cdr) => {
                let car = self.eval(env.clone(), car.clone());

                match &*car {
                    &Expr::SpecialForm { func, .. } => {
                        let children = match self.sexp_to_list(cdr.clone()) {
                            Some(it) => it,
                            None => {
                                return self.make_err(
                                    "application: cdr must be a proper list".to_string(),
                                    Some(cdr.clone()),
                                )
                            }
                        };
                        func(self, env, &children)
                    }
                    &Expr::NativeProcedure { func, .. } => {
                        let args = match self.sexp_to_list(cdr.clone()) {
                            Some(it) => it,

                            None => {
                                return self.make_err(
                                    "application: cdr must be a proper list".to_string(),
                                    Some(cdr.clone()),
                                )
                            }
                        };

                        let evaled_args = args
                            .into_iter()
                            .map(|expr| self.eval(env.clone(), expr))
                            .collect::<Vec<_>>();

                        func(self, &evaled_args)
                    }
                    Expr::Procedure {
                        args: arg_names,
                        body,
                        env: closed_env,
                        variadic,
                    } => {
                        // Fill the arg slots via a new namespace
                        let mut arg_env = Namespace::new(closed_env.clone());

                        let args_passed = match self.sexp_to_list(cdr.clone()) {
                            Some(it) => it,
                            None => {
                                return self.make_err(
                                    "application: cdr must be a proper list".to_string(),
                                    Some(cdr.clone()),
                                )
                            }
                        };
                        // Eval the args in the parent context
                        let args_passed_evaled = args_passed
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
                                return self.make_err(message, Some(data));
                            }
                        }

                        let arg_env = Gc::new(GcCell::new(arg_env));

                        let result = body
                            .iter()
                            .map(|expr| self.eval(arg_env.clone(), expr.clone()))
                            .last();
                        result.unwrap_or_else(|| {
                            self.make_err(
                                "application: had a procedure with no body sexprs".to_string(),
                                None,
                            )
                        })
                    }
                    _ => self.make_err("application: not a procedure".to_string(), Some(car)),
                }
            }
        }
    }
}

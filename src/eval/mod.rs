use crate::{Engine, Expr, Namespace};

mod thtd;
use generational_arena::Index;
pub use thtd::add_thtandard_library;

impl Engine {
    /// Evaluate the expression at the given location on the heap,
    /// put the result on the heap, and return it.
    pub fn eval(&mut self, env: &mut Namespace, expr_idx: Index) -> Index {
        let expr = self.get(expr_idx);

        match expr {
            // Passthru literals unchanged
            Expr::Integer(_) | Expr::Nil | Expr::String(_) | Expr::NativeFunction { .. } => {
                expr_idx
            }
            // Lookup the symbol
            &Expr::Symbol(id) => {
                let idx = self.lookup_symbol(id, env);
                idx.unwrap_or_else(|| self.get_symbol_index("!"))
            }
            &Expr::Pair(car, cdr) => {
                let quote_id = self.intern_symbol("quote");

                let car = self.eval(env, car);
                let car = self.get(car);

                match *car {
                    Expr::Symbol(id) if id == quote_id => cdr,
                    Expr::NativeFunction { func, .. } => {
                        let children = match self.sexp_to_vector(cdr) {
                            Some(it) => it,
                            None => return self.get_symbol_index("!"),
                        };
                        let res = func(self, env, &children);
                        self.insert(res)
                    }
                    _ => self.get_symbol_index("!"),
                }
            }
        }
    }
}

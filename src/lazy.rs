//! Lazy cons cells for iterator-like lists.

use super::*;

use gc::{Gc, GcCell};

impl Engine {
    /// Eval a LazyExprCell.
    pub fn eval_cell(&mut self, env: &Gc<GcCell<Namespace>>, expr: &LazyExprCell) -> EvalResult {
        let (expr, done) = &mut *expr.borrow_mut();
        if *done {
            Ok(expr.clone())
        } else {
            *done = true;
            let evaluated = self.eval_inner(env.clone(), expr.clone());
            *expr = match &evaluated {
                Ok(val) => val.clone(),
                Err(_) => Gc::new(Expr::Nil),
            };
            evaluated
        }
    }

    /// Split a cons pair into head and tail
    pub fn car(&mut self, expr: Gc<Expr>) -> EvalResult {
        match &*expr {
            Expr::Pair(car, _) => Ok(car.to_owned()),
            Expr::LazyPair(car, _, ctx) => self.eval_cell(ctx, car),
            _ => Err(crate::eval::thtd::bad_arg_type(self, expr, 0, "pair")),
        }
    }
    pub fn cdr(&mut self, expr: Gc<Expr>) -> EvalResult {
        match &*expr {
            Expr::Pair(_, cdr) => Ok(cdr.to_owned()),
            Expr::LazyPair(_, cdr, ctx) => self.eval_cell(ctx, cdr),
            _ => Err(crate::eval::thtd::bad_arg_type(self, expr, 0, "pair")),
        }
    }
    pub fn split_cons_verb(
        &mut self,
        expr: Gc<Expr>,
    ) -> Result<Option<(Gc<Expr>, Gc<Expr>)>, Exception> {
        let val = match &*expr {
            Expr::Pair(car, cdr) => (car.to_owned(), cdr.to_owned()),
            Expr::LazyPair(car, cdr, ctx) => (self.eval_cell(ctx, car)?, self.eval_cell(ctx, cdr)?),
            _ => return Ok(None),
        };
        Ok(Some(val))
    }
    pub fn split_cons(&mut self, expr: Gc<Expr>) -> Result<(Gc<Expr>, Gc<Expr>), Exception> {
        match self.split_cons_verb(expr.clone())? {
            Some(pair) => Ok(pair),
            None => Err(crate::eval::thtd::bad_arg_type(self, expr, 0, "pair")),
        }
    }
    /// Reify an expr by evaluating all contained LazyExprCells.
    pub fn reify(&mut self, expr: &Gc<Expr>) -> Result<(), Exception> {
        if let Some((car, cdr)) = &self.split_cons_verb(expr.clone())? {
            self.reify(car)?;
            self.reify(cdr)?;
        }
        Ok(())
    }
}

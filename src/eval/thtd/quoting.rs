//! Quotes, unquotes, and quasiquotes.
use super::*;
use crate::eval::TailRec;

/// Note this is the "function" quote which should just return its argument.
pub fn quote(engine: &mut Engine, _env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    TailRec::Exit(if let Err(e) = check_argc(engine, args, 1, 1) {
        e
    } else {
        args[0].to_owned()
    })
}

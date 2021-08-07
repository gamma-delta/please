//! Quotes, unquotes, and quasiquotes.
use super::*;

/// Note this is the "function" quote which should just return its argument.
pub fn quote(engine: &mut Engine, _env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 1, 1) {
        return e;
    }

    args[0].to_owned()
}

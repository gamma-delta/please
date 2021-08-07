//! Quotes, unquotes, and quasiquotes.
use super::*;
use crate::eval::TailRec;

/// Note this is the "function" quote which should just return its argument.
pub fn quote(engine: &mut Engine, _env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Result<Gc<Expr>, TailRec> {
    if let Err(e) = check_argc(engine, args, 1, 1) {
        return Ok(e);
    }

    Ok(args[0].to_owned())
}

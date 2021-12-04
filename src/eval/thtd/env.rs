//! Messing with the environment and namespaces.

use super::*;
use crate::eval::TailRec;
use crate::Expr;

pub fn define(
    engine: &mut Engine,
    env: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> Result<TailRec, Exception> {
    check_argc(engine, args, 2, 2)?;

    let spec = args[0].to_owned();
    let val = args[1].to_owned();
    let bindings = engine.destructure_assign(spec, val.to_owned())?;
    env.borrow_mut().merge_from(bindings);

    Ok(TailRec::Exit(val))
}

//! Misc functions ofc

use super::*;
use crate::eval::TailRec;

pub fn loop_(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    if let Err(e) = check_min_argc(engine, args, 2) {
        return TailRec::Exit(e);
    }

    todo!()
}

//! Messing with the environment and namespaces.
use super::*;
use crate::eval::TailRec;

pub fn define(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> TailRec {
    if let Err(e) = check_argc(engine, args, 2, 2) {
        return TailRec::Exit(e);
    }

    let name = match &*args[0] {
        Expr::Symbol(id) => *id,
        _ => return TailRec::Exit(bad_arg_type(engine, args[0].clone(), 0, "symbol")),
    };
    let rhs = engine.eval(env.clone(), args[1].clone());

    env.borrow_mut().insert(name, rhs);

    TailRec::Exit(Gc::new(Expr::Nil))
}

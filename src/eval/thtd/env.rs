//! Messing with the environment and namespaces.
use super::*;

pub fn define(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 2, 2) {
        return e;
    }

    let name = match &*args[0] {
        Expr::Symbol(id) => *id,
        _ => return bad_arg_type(engine, args[0].clone(), 0, "symbol"),
    };
    let rhs = engine.eval(env.clone(), args[1].clone());

    env.borrow_mut().insert(name, rhs);

    Gc::new(Expr::Nil)
}

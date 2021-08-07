//! Messing with strings.
use super::*;

pub fn to_string(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 1, 1) {
        return e;
    }
    let string = engine.write_expr(args[0].clone());
    Gc::new(Expr::String(string))
}

pub fn prn(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(e) = check_argc(engine, args, 1, 2) {
        return e;
    }

    let newline = if let Some(check) = args.get(1) {
        engine.is_truthy(check.to_owned())
    } else {
        true
    };

    let out = engine.print_expr(args[0].clone());

    if newline {
        println!("{}", out);
    } else {
        print!("{}", out);
        std::io::stdout().flush().unwrap();
    }

    Gc::new(Expr::Nil)
}

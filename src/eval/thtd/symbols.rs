//! Manipulating symbols
use super::*;

pub fn symbol2string(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_argc(engine, args, 1, 1) {
        return ono;
    };

    let arg = args[0].to_owned();
    let sym = if let Expr::Symbol(sym) = &*arg {
        *sym
    } else {
        return bad_arg_type(engine, arg, 0, "symbol");
    };
    let string = engine
        .get_symbol_str(sym)
        .unwrap_or("<unknown>")
        .to_string();
    Gc::new(Expr::String(string))
}

pub fn string2symbol(engine: &mut Engine, args: &[Gc<Expr>]) -> Gc<Expr> {
    if let Err(ono) = check_argc(engine, args, 1, 1) {
        return ono;
    };

    let arg = args[0].to_owned();
    let string = if let Expr::String(s) = &*arg {
        s
    } else {
        return bad_arg_type(engine, arg, 0, "symbol");
    };
    let sym = engine.intern_symbol(string);

    Gc::new(Expr::Symbol(sym))
}

//! Manipulating symbols
use super::*;

pub fn symbol2string(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let arg = args[0].to_owned();
    let sym = if let Expr::Symbol(sym) = &*arg {
        *sym
    } else {
        return Err(bad_arg_type(engine, arg, 0, "symbol"));
    };
    let string = engine
        .get_symbol_str(sym)
        .unwrap_or("<unknown>")
        .to_string();
    Ok(Gc::new(Expr::String(string.into_bytes())))
}

pub fn string2symbol(
    engine: &mut Engine,
    _: Gc<GcCell<Namespace>>,
    args: &[Gc<Expr>],
) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let arg = args[0].to_owned();
    let string = if let Expr::String(s) = &*arg {
        s
    } else {
        return Err(bad_arg_type(engine, arg, 0, "string"));
    };
    let sym = engine.intern_symbol(&String::from_utf8_lossy(string));

    Ok(Gc::new(Expr::Symbol(sym)))
}

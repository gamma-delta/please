use super::*;

pub fn read_file(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    let arg = args[0].to_owned();

    let path_stub = if let Expr::String(s) = &*arg {
        s
    } else {
        return Err(bad_arg_type(engine, arg, 0, "path"));
    };

    // faux try block
    let res = (|| {
        let path = std::env::current_dir()?.join(String::from_utf8_lossy(path_stub).as_ref());
        std::fs::read(path)
    })();
    match res {
        Ok(s) => EvalResult::Ok(Gc::new(Expr::String(s))),
        Err(oh_no) => {
            let msg = oh_no.to_string();
            EvalResult::Err(engine.make_err(
                "read-file/io-err",
                format!("os-level error: {}", &msg),
                Some(Gc::new(Expr::String(msg.into_bytes()))),
            ))
        }
    }
}

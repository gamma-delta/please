//! The worst part of every AoC is getting the input in.

use itertools::Itertools;

use super::*;

pub fn scanf(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    let mut data = if let Expr::String(s) = &*args[0] {
        s.as_slice()
    } else {
        return Err(bad_arg_type(engine, args[0].to_owned(), 0, "string"));
    };

    let mut fmt = if let Expr::String(s) = &*args[1] {
        s.as_slice()
    } else {
        return Err(bad_arg_type(engine, args[1].to_owned(), 1, "string"));
    };

    let mut datums_read = Vec::new();

    while !data.is_empty() {
        let c = read_char(&mut fmt).ok_or_else(|| {
            engine.make_err(
                "scanf/exhausted-format-chars",
                "ran out of formatting chars when there was still leftover data".to_owned(),
                None,
            )
        })?;

        match c {
            b'%' => {
                let spec = read_char(&mut fmt).ok_or_else(|| {
                    engine.make_err(
                        "scanf/missing-format",
                        "expected formatting char(s) after a '%'".to_owned(),
                        None,
                    )
                })?;
                match spec {
                    b'i' => {
                        let end = data
                            .iter()
                            .position(|&b: &u8| {
                                !((b'0'..=b'9').contains(&b) || b == b'+' || b == b'-')
                            })
                            .unwrap_or_else(|| data.len());
                        let (s, rest) = data.split_at(end);
                        data = rest;

                        let parsable = String::from_utf8_lossy(s);
                        let num = parsable.parse::<i64>().map_err(|e| {
                            engine.make_err(
                                "scanf/int-fail",
                                format!("could not parse int {}: {}", parsable, e),
                                Some(Engine::list_to_sexp(&[
                                    Gc::new(Expr::String(s.to_owned())),
                                    Gc::new(Expr::String(e.to_string().into_bytes())),
                                ])),
                            )
                        })?;
                        datums_read.push(Gc::new(Expr::Integer(num)));
                    }
                    b's' => {
                        let end = data
                            .iter()
                            .position(|b| *b == b' ')
                            .unwrap_or_else(|| data.len());
                        let (s, rest) = data.split_at(end);
                        data = rest;

                        datums_read.push(Gc::new(Expr::String(s.to_owned())));
                    }
                    b'[' => {
                        let negate = fmt.starts_with(b"^");
                        let set_end = fmt.iter().position(|b| *b == b']').ok_or_else(|| {
                            engine.make_err(
                                "scanf/no-charset-end",
                                "a charset \"%[chars]\" requires an ending bracket".to_string(),
                                None,
                            )
                        })?;

                        let (set, rest) = fmt.split_at(set_end);
                        let set = set.iter().dedup().collect_vec();
                        fmt = &rest[1..]; // skip the one-byte '['

                        // We actually search for the first char that *doesn't* match
                        let ending_idx = data
                            .iter()
                            .position(|b: &u8| {
                                if negate {
                                    // search for something *in* the set to end it
                                    set.contains(&b)
                                } else {
                                    // search for something *out* of the set to end it
                                    !set.contains(&b)
                                }
                            })
                            .unwrap_or_else(|| data.len());
                        let (s, rest) = data.split_at(ending_idx);
                        data = rest;
                        datums_read.push(Gc::new(Expr::String(s.to_owned())));
                    }

                    ono => {
                        // ughh
                        return Err(engine.make_err(
                            "scanf/bad-specifier",
                            format!("the format char {:?} is invalid", ono as char),
                            Some(Gc::new(Expr::String(vec![ono]))),
                        ));
                    }
                }
            }
            b' ' => {
                // Match any number of non-whitespace chars including zero
                // if we never find a non-whitespace, read the whole string
                let next_non_ws = data
                    .iter()
                    .position(|b| *b != b' ')
                    .unwrap_or_else(|| data.len());
                let (_ws, rest) = data.split_at(next_non_ws);
                data = rest;
            }
            c => {
                let next_c = read_char(&mut data).ok_or_else(|| {
                    engine.make_err(
                        "scanf/exhausted-data-chars",
                        "ran out of data chars when there was still leftover format".to_owned(),
                        None,
                    )
                })?;
                if next_c != c {
                    // o no
                    return Err(engine.make_err(
                        "scanf/mismatched-chars",
                        format!(
                            "the format string expected {:?} but the data string had {:?}",
                            c, next_c
                        ),
                        Some(Engine::list_to_sexp(&[
                            Gc::new(Expr::String(c.to_string().into_bytes())),
                            Gc::new(Expr::String(next_c.to_string().into_bytes())),
                        ])),
                    ));
                } // else data has already been advanced yahoo
            }
        }
    }

    Ok(Engine::list_to_sexp(&datums_read))
}

fn read_char(s: &mut &[u8]) -> Option<u8> {
    s.first().map(|c| {
        *s = &s[1..];
        *c
    })
}

pub fn read(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let s = match &*args[0] {
        Expr::String(s) => s,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "string")),
    };

    let res = match crate::parse::read_many(
        String::from_utf8_lossy(s).as_ref(),
        "<read>".to_owned(),
        engine,
    ) {
        Ok(it) => it.into_iter().map(Gc::new).collect_vec(),
        Err(ono) => return Err(engine.make_err("read/syntax", ono.to_string(), None)),
    };
    Ok(Engine::list_to_sexp(&res))
}

pub fn write(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    engine
        .write_expr(args[0].to_owned())
        .map(|s| Gc::new(Expr::String(s.into_bytes())))
}

pub fn native_repr(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;
    let repr = format!("{:?}", &args[0]);
    Ok(Expr::string(repr.into_bytes()))
}

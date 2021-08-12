//! The worst part of every AoC is getting the input in.

use super::*;

pub fn scanf(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 2, 2)?;

    let mut data = if let Expr::String(s) = &*args[0] {
        s.as_str()
    } else {
        return Err(bad_arg_type(engine, args[0].to_owned(), 0, "string"));
    };

    let mut fmt = if let Expr::String(s) = &*args[1] {
        s.as_str()
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
            '%' => {
                let spec = read_char(&mut fmt).ok_or_else(|| {
                    engine.make_err(
                        "scanf/missing-format",
                        "expected formatting char(s) after a '%'".to_owned(),
                        None,
                    )
                })?;
                match spec {
                    'i' => {
                        let end = data
                            .find(|c: char| !(('0'..='9').contains(&c) || c == '+' || c == '-'))
                            .unwrap_or_else(|| data.len());
                        let (s, rest) = data.split_at(end);
                        data = rest;

                        let num = s.parse::<i64>().map_err(|e| {
                            engine.make_err(
                                "scanf/int-fail",
                                format!("could not parse int {}: {}", s, e),
                                Some(Engine::list_to_sexp(&[
                                    Gc::new(Expr::String(s.to_owned())),
                                    Gc::new(Expr::String(e.to_string())),
                                ])),
                            )
                        })?;
                        datums_read.push(Gc::new(Expr::Integer(num)));
                    }
                    's' => {
                        let end = data.find(char::is_whitespace).unwrap_or_else(|| data.len());
                        let (s, rest) = data.split_at(end);
                        data = rest;

                        datums_read.push(Gc::new(Expr::String(s.to_owned())));
                    }

                    ono => {
                        return Err(engine.make_err(
                            "scanf/bad-specifier",
                            format!("the format char {:?} is invalid", ono),
                            Some(Gc::new(Expr::String(ono.to_string()))),
                        ));
                    }
                }
            }
            c if c.is_whitespace() => {
                // Match any number of non-whitespace chars including zero
                // if we never find a non-whitespace, read the whole string
                let next_non_ws = data
                    .find(|c: char| !c.is_whitespace())
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
                            Gc::new(Expr::String(c.to_string())),
                            Gc::new(Expr::String(next_c.to_string())),
                        ])),
                    ));
                } // else data has already been advanced yahoo
            }
        }
    }

    Ok(Engine::list_to_sexp(&datums_read))
}

fn read_char(s: &mut &str) -> Option<char> {
    let c = s.chars().next();
    c.map(|c| {
        *s = &s[c.len_utf8()..];
        c
    })
}

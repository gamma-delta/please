use std::{borrow::Cow, ops::Deref};

use crate::hash::GcMap;

use super::*;

pub fn peg_match(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    // Tired of adding new args to this
    #[non_exhaustive]
    struct RecurCtx<'map, 'text> {
        full_text: &'text [u8],
        env: Gc<GcCell<Namespace>>,
        map_ctx: Option<&'map GcMap>,
    }
    /// Return the number of chars matched and a capture stack, or None if it doesn't.
    ///
    /// Todo: tail recursion?
    #[allow(clippy::type_complexity)]
    fn recur(
        engine: &mut Engine,
        peg: Gc<Expr>,
        cursor: usize,
        ctx: &RecurCtx,
    ) -> Result<Option<(usize, Vec<Gc<Expr>>)>, Exception> {
        let text = &ctx.full_text[cursor..];
        Ok(match &*peg {
            &Expr::Integer(i) => {
                if i >= 0 && (i as usize) <= text.len() {
                    Some((i as usize, vec![]))
                } else {
                    None
                }
            }
            Expr::String(s) => {
                if text.starts_with(s) {
                    Some((s.len(), vec![]))
                } else {
                    None
                }
            }
            Expr::Pair(head, tail) if matches!(&**head, Expr::Symbol(_)) => {
                let op_sym = match &**head {
                    Expr::Symbol(s) => *s,
                    _ => unreachable!(),
                };
                let op = engine.get_symbol_str(op_sym).unwrap().to_owned();
                let body = engine.sexp_to_list(tail.to_owned())?.ok_or_else(|| {
                    engine.make_err(
                        "peg/match/improper-list",
                        "requires a proper list as the tail of a pattern".to_owned(),
                        Some(tail.to_owned()),
                    )
                })?;

                match op.as_str() {
                    "range" => {
                        let c = if let Some(c) = text.first() {
                            *c
                        } else {
                            return Ok(None);
                        };
                        let ranges = body
                            .into_iter()
                            .map(|expr| {
                                let s = match &*expr {
                                    Expr::String(s) => s,
                                    _ => {
                                        return Err(engine.make_err(
                                            "peg/match/range/non-string",
                                            "range arguments must be 2-byte strings".to_string(),
                                            Some(expr),
                                        ))
                                    }
                                };
                                let (start, end) = match s.as_slice() {
                                    [start, end] => (*start, *end),
                                    _ => {
                                        return Err(engine.make_err(
                                            "peg/match/range/bad-len",
                                            "range arguments must be 2-byte strings".to_string(),
                                            Some(expr),
                                        ))
                                    }
                                };
                                Ok(start..=end)
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        for range in ranges {
                            if range.contains(&c) {
                                return Ok(Some((1, vec![])));
                            }
                        }
                        None
                    }
                    "set" => {
                        let c = if let Some(c) = text.first() {
                            *c
                        } else {
                            return Ok(None);
                        };

                        let set = if let [set] = body.as_slice() {
                            set
                        } else {
                            return Err(engine.make_err(
                                "peg/match/set/argc",
                                "set takes one string argument".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        let set = match &**set {
                            Expr::String(s) => s,
                            _ => {
                                return Err(engine.make_err(
                                    "peg/match/set/non-string",
                                    "set takes one string argument".to_string(),
                                    Some(set.to_owned()),
                                ))
                            }
                        };

                        if set.contains(&c) {
                            Some((1, vec![]))
                        } else {
                            None
                        }
                    }
                    "choice" | "+" => {
                        for attempt in body {
                            if let Some(it) = recur(engine, attempt, cursor, ctx)? {
                                return Ok(Some(it));
                            }
                        }
                        None
                    }
                    "sequence" | "*" => {
                        let mut subcursor = 0;
                        let mut stack = vec![];
                        for subexpr in body {
                            match recur(engine, subexpr, cursor + subcursor, ctx) {
                                Ok(Some((len, substack))) => {
                                    subcursor += len;
                                    stack.extend(substack);
                                }
                                ono @ (Ok(None) | Err(_)) => {
                                    // if we match none, get out of here
                                    // if there's an error, also get out of here
                                    return ono;
                                }
                            }
                        }

                        Some((subcursor, stack))
                    }
                    "any" | "some" | "between" | "at-least" | "at-most" | "opt" | "?" => {
                        let (min, max, subpeg) = match op.as_str() {
                            "any" | "some" => {
                                let subpeg = if let [subpeg] = body.as_slice() {
                                    subpeg.to_owned()
                                } else {
                                    return Err(engine.make_err(
                                        &format!("peg/{}/argc", op),
                                        format!("{} requires 1 peg argument", op),
                                        Some(tail.to_owned()),
                                    ));
                                };
                                let min = if op == "any" { 0i64 } else { 1 };
                                (Some(min), None, subpeg)
                            }
                            "between" => {
                                let (min, max, subpeg) = if let [min, max, subpeg] = body.as_slice()
                                {
                                    (min, max, subpeg.to_owned())
                                } else {
                                    return Err(engine.make_err(
                                        "peg/between/argc",
                                        "between requires 2 int and one peg argument".to_string(),
                                        Some(tail.to_owned()),
                                    ));
                                };
                                let min = match &**min {
                                    Expr::Integer(x) => *x,
                                    _ => {
                                        return Err(engine.make_err(
                                            "peg/between/argc",
                                            "between requires 2 int and one peg argument"
                                                .to_string(),
                                            Some(min.to_owned()),
                                        ))
                                    }
                                };
                                let max = match &**max {
                                    Expr::Integer(x) => *x,
                                    _ => {
                                        return Err(engine.make_err(
                                            "peg/between/argc",
                                            "between requires 2 int and one peg argument"
                                                .to_string(),
                                            Some(max.to_owned()),
                                        ))
                                    }
                                };
                                (Some(min), Some(max), subpeg)
                            }
                            "at-least" | "at-most" => {
                                let (n, subpeg) = if let [n, subpeg] = body.as_slice() {
                                    (n, subpeg.to_owned())
                                } else {
                                    return Err(engine.make_err(
                                        &format!("peg/{}/argc", op),
                                        format!("{} requires one int and one peg argument", op),
                                        Some(tail.to_owned()),
                                    ));
                                };
                                let n = match &**n {
                                    Expr::Integer(x) => *x,
                                    _ => {
                                        return Err(engine.make_err(
                                            &format!("peg/{}/argc", op),
                                            format!("{} requires one int and one peg argument", op),
                                            Some(tail.to_owned()),
                                        ))
                                    }
                                };
                                let range = if op == "at-least" {
                                    (Some(n), None)
                                } else {
                                    (None, Some(n))
                                };
                                (range.0, range.1, subpeg)
                            }
                            "opt" | "?" => {
                                let subpeg = if let [subpeg] = body.as_slice() {
                                    subpeg.to_owned()
                                } else {
                                    return Err(engine.make_err(
                                        "peg/opt/argc",
                                        "opt requires 1 peg argument".to_string(),
                                        Some(tail.to_owned()),
                                    ));
                                };
                                (Some(0), Some(1), subpeg)
                            }
                            _ => unreachable!("i forgor to impl {}", op),
                        };

                        let too_small = |x: i64| {
                            if let Some(min) = min {
                                x < min
                            } else {
                                false
                            }
                        };
                        let too_big = |x: i64| {
                            if let Some(max) = max {
                                x > max
                            } else {
                                false
                            }
                        };

                        let mut count = 0;
                        let mut subcursor = 0;
                        let mut substack = vec![];
                        loop {
                            match recur(engine, subpeg.to_owned(), cursor + subcursor, ctx) {
                                Ok(Some((len, stk))) => {
                                    count += 1;
                                    // have we matched 1 too many times?
                                    if too_big(count) {
                                        // oh no :(
                                        break None;
                                    }

                                    subcursor += len;
                                    substack.extend(stk);
                                }
                                Ok(None) => {
                                    // this time we did *not* match, so we quit.
                                    // check if we're in bounds.
                                    break if too_small(count) {
                                        // oh no, we matched too few times.
                                        // note we'll never be too big because we always check if we're
                                        // too big above
                                        None
                                    } else {
                                        // good ending!
                                        Some((subcursor, substack))
                                    };
                                }
                                // and of course exns need not apply
                                ono @ Err(_) => return ono,
                            }
                        }
                    }

                    "capture" | "<-" | "quote" => {
                        let subpeg = if let [subpeg] = body.as_slice() {
                            subpeg.to_owned()
                        } else {
                            return Err(engine.make_err(
                                "peg/capture/argc",
                                "capture takes 1 PEG argument".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        recur(engine, subpeg, cursor, ctx)?.map(|(len, mut stack)| {
                            let matched = &text[..len];
                            stack.push(Expr::string(matched.to_owned()));
                            (len, stack)
                        })
                    }
                    "replace" | "/" => {
                        let (subpeg, replacer) = if let [subpeg, subst] = body.as_slice() {
                            (subpeg.to_owned(), subst.to_owned())
                        } else {
                            return Err(engine.make_err(
                                "peg/replace/argc",
                                "replace takes 1 PEG and 1 any argument".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        match recur(engine, subpeg, cursor, ctx)? {
                            Some((len, stack)) => {
                                let replacer = engine.eval_inner(ctx.env.to_owned(), replacer)?;
                                let replacement = if replacer.is_callable() {
                                    let sexp = Expr::pair(replacer, Engine::list_to_sexp(&stack));
                                    engine.eval_inner(ctx.env.to_owned(), sexp)?
                                } else {
                                    replacer
                                };
                                Some((len, vec![replacement]))
                            }
                            None => None,
                        }
                    }
                    "group" => {
                        let subpeg = if let [subpeg] = body.as_slice() {
                            subpeg.to_owned()
                        } else {
                            return Err(engine.make_err(
                                "peg/group/argc",
                                "group takes 1 PEG argument".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        recur(engine, subpeg, cursor, ctx)?
                            .map(|(len, stack)| (len, vec![Engine::list_to_sexp(&stack)]))
                    }
                    "if" => {
                        let (subpeg, then) = if let [subpeg, then] = body.as_slice() {
                            (subpeg.to_owned(), then.to_owned())
                        } else {
                            return Err(engine.make_err(
                                "peg/if/argc",
                                "if takes 2 PEG arguments".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        match recur(engine, subpeg, cursor, ctx) {
                            Ok(Some(_)) => {
                                // we don't care what it was lmao
                                recur(engine, then, cursor, ctx)?
                            }
                            ono => ono?,
                        }
                    }
                    "if-not" => {
                        let (subpeg, els) = if let [subpeg, els] = body.as_slice() {
                            (subpeg.to_owned(), els.to_owned())
                        } else {
                            return Err(engine.make_err(
                                "peg/if-not/argc",
                                "if-not takes 2 PEG arguments".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        match recur(engine, subpeg, cursor, ctx) {
                            Ok(None) => {
                                // we failed, woot
                                recur(engine, els, cursor, ctx)?
                            }
                            ono => ono?,
                        }
                    }
                    "not" | "!" => {
                        let subpeg = if let [subpeg] = body.as_slice() {
                            subpeg.to_owned()
                        } else {
                            return Err(engine.make_err(
                                "peg/not/argc",
                                "not takes 1 PEG argument".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        match recur(engine, subpeg, cursor, ctx) {
                            Ok(None) => {
                                // we failed, woot
                                Some((0, vec![]))
                            }
                            ono => ono?,
                        }
                    }
                    "position" | "$" => {
                        if body.is_empty() {
                            Some((0, vec![Expr::integer(cursor as i64)]))
                        } else {
                            return Err(engine.make_err(
                                "peg/position/argc",
                                "position takes 0 arguments".to_string(),
                                Some(tail.to_owned()),
                            ));
                        }
                    }
                    "all" => {
                        let subpeg = if let [subpeg] = body.as_slice() {
                            subpeg.to_owned()
                        } else {
                            return Err(engine.make_err(
                                "peg/all/argc",
                                "all takes 1 PEG argument".to_string(),
                                Some(tail.to_owned()),
                            ));
                        };
                        match recur(engine, subpeg, cursor, ctx) {
                            Ok(Some((len, stack))) => {
                                // We're only OK if we matched the *entirety* of the remaining string
                                if len == text.len() {
                                    Some((len, stack))
                                } else {
                                    None
                                }
                            }
                            ono => ono?,
                        }
                    }

                    _ => {
                        return Err(engine.make_err(
                            "peg/match/bad-op",
                            format!("did not recognize the operator '{}", op),
                            Some(Gc::new(Expr::String(op.as_bytes().to_owned()))),
                        ));
                    }
                }
            }
            Expr::Map(map) => {
                let main_sym = engine.intern_symbol("main");
                let main_peg = match map.get(&Expr::Symbol(main_sym)) {
                    Some(peg) => peg.to_owned(),
                    _ => {
                        return Err(engine.make_err(
                            "peg/match/map/no-main",
                            "a map PEG requires a 'main key as an entry point".to_string(),
                            Some(peg),
                        ))
                    }
                };

                let ctx = RecurCtx {
                    map_ctx: Some(map),
                    env: ctx.env.to_owned(),
                    ..*ctx
                };
                recur(engine, main_peg, cursor, &ctx)?
            }
            Expr::Symbol(sym) => {
                let local_map = ctx.map_ctx.map(Cow::Borrowed);
                let global_map_sym = engine.intern_symbol("peg/default-grammar");
                let global_map = ctx.env.borrow().lookup(global_map_sym).and_then(|expr| {
                    if let Expr::Map(m) = &*expr {
                        Some(Cow::Owned(m.clone()))
                    } else {
                        None
                    }
                });
                for map in local_map.iter().chain(global_map.iter()) {
                    // Do a lookup in the map?
                    if let Some(subpeg) = map.get(&Expr::Symbol(*sym)) {
                        // yoo we found it
                        return recur(engine, subpeg.to_owned(), cursor, ctx);
                    }
                }

                return Err(engine.make_err(
                    "peg/match/no-lookup",
                    format!(
                        "could not find a PEG associated with '{}",
                        engine.get_symbol_str(*sym).unwrap()
                    ),
                    Some(peg),
                ));
            }
            _ => {
                return Err(engine.make_err(
                    "peg/match/bad-type",
                    format!("cannot use a {} as a matcher", peg.type_name()),
                    Some(peg),
                ))
            }
        })
    }

    check_argc(engine, args, 2, 3)?;
    let peg = args[0].to_owned();
    let s = match &*args[1] {
        Expr::String(s) => s,
        _ => return Err(bad_arg_type(engine, args[1].to_owned(), 1, "string")),
    };
    let start = match args.get(2).map(Deref::deref) {
        Some(Expr::Integer(i)) if *i > 0 => *i as usize,
        None => 0,
        Some(_) => {
            return Err(bad_arg_type(
                engine,
                args[2].to_owned(),
                2,
                "optional positive integer",
            ))
        }
    };

    let matched = recur(
        engine,
        peg,
        start,
        &RecurCtx {
            full_text: s,
            env,
            map_ctx: None,
        },
    )?;
    Ok(match matched {
        Some((_, captures)) => Engine::list_to_sexp(&captures),
        None => Expr::bool(false),
    })
}

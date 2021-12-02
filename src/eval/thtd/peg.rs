use super::*;

pub fn peg_match(engine: &mut Engine, _: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    /// Return the number of chars matched or 0 if it doesn't.
    fn recur(
        engine: &mut Engine,
        peg: Gc<Expr>,
        text: &[u8],
        stack: &mut Vec<Vec<u8>>,
    ) -> Result<Option<usize>, Exception> {
        Ok(match &*peg {
            Expr::Integer(i) if *i >= 0 => Some(*i as usize),
            Expr::String(s) => {
                if text.starts_with(s) {
                    Some(s.len())
                } else {
                    None
                }
            }
            Expr::Pair(head, tail) if matches!(&**head, Expr::Symbol(_)) => {
                let op = match &**head {
                    Expr::Symbol(s) => engine.get_symbol_str(*s).unwrap().to_owned(),
                    _ => unreachable!(),
                };
                let body = engine.sexp_to_list(tail.to_owned())?.ok_or_else(|| {
                    engine.make_err(
                        "peg/match/improper-list",
                        "require a proper list as the tail of a pattern".to_owned(),
                        Some(tail.to_owned()),
                    )
                })?;

                match op.as_str() {
                    "choice" | "+" => {
                        for attempt in body {
                            if let Some(it) = recur(engine, attempt, text, stack)? {
                                return Ok(Some(it));
                            }
                        }
                        None
                    }
                    "sequence" | "*" => {
                        todo!()
                    }
                    _ => {
                        return Err(engine.make_err(
                            "peg/match/bad-op",
                            format!("did not recognize the op '{}", op),
                            Some(Gc::new(Expr::String(op.as_bytes().to_owned()))),
                        ))
                    }
                }
            }
            _ => {
                return Err(engine.make_err(
                    "peg/match/bad-type",
                    "could not use this as a match".to_owned(),
                    Some(peg),
                ))
            }
        })
    }

    todo!()
}

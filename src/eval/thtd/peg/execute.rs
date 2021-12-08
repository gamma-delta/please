use std::{convert::TryInto, slice::SliceIndex};

use gc::{Gc, GcCell};
use itertools::Itertools;

use crate::{
    eval::{
        bad_arg_type,
        thtd::{check_argc, check_min_argc},
    },
    Engine, EvalResult, Exception, Expr, Namespace, Value,
};

use super::{Opcode, BYTE, BYTE_SIZE, U, U_SIZE};

pub fn match_(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Value]) -> EvalResult {
    check_argc(engine, args, 2, 4)?;

    let peg = match engine.sexp_to_list(args[0].to_owned())? {
        Some(it) => it,
        None => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "PEG")),
    };
    let (peg_sym, bytecode, exprs) = if let [peg_sym, bytecode, exprs @ ..] = peg.as_slice() {
        (peg_sym, bytecode, exprs)
    } else {
        return Err(bad_arg_type(engine, args[0].to_owned(), 0, "PEG"));
    };
    let reference_sym = engine.intern_symbol("peg");
    if peg_sym != &Expr::symbol(reference_sym) {
        return Err(bad_arg_type(engine, args[0].to_owned(), 0, "PEG"));
    }
    let bytecode = match &**bytecode {
        Expr::String(s) => s,
        _ => return Err(bad_arg_type(engine, args[0].to_owned(), 0, "PEG")),
    };

    let text = match &*args[1] {
        Expr::String(s) => s,
        _ => return Err(bad_arg_type(engine, args[1].to_owned(), 1, "string")),
    };

    let start_cursor = if let Some(cursor) = args.get(2) {
        match **cursor {
            Expr::Integer(cursor) if cursor >= 0 => cursor,
            _ => {
                return Err(bad_arg_type(
                    engine,
                    args[2].to_owned(),
                    2,
                    "optional positive integer",
                ))
            }
        }
    } else {
        0
    };

    let debug = if let Some(dbg) = args.get(3) {
        engine.is_truthy(dbg.clone())
    } else {
        false
    };

    if debug {
        println!("{}", engine.write_expr(args[0].to_owned()).unwrap());
    }

    let mut executor = Executor {
        bytecode,
        exprs: exprs.to_owned(),
        engine,
        env,
        debug,
    };
    let matched = executor.match_(0, text, start_cursor as usize)?;
    Ok(match matched {
        Some((_, stack)) => Engine::list_to_sexp(&stack),
        None => Expr::bool(false),
    })
}

/// eeeexxecutor!
struct Executor<'code, 'engine> {
    bytecode: &'code [u8],
    exprs: Vec<Value>,

    engine: &'engine mut Engine,
    env: Gc<GcCell<Namespace>>,

    debug: bool,
}

impl<'code> Executor<'code, '_> {
    fn match_(
        &mut self,
        mut bytecode_cursor: usize,
        full_text: &[u8],
        text_cursor: usize,
    ) -> Result<Option<(usize, Vec<Value>)>, Exception> {
        let text = &full_text[text_cursor..];
        let opc = self.read_opcode(&mut bytecode_cursor)?;

        if self.debug {
            println!(
                "^^^ matching {:?} at textidx {}, args at {}",
                opc, text_cursor, bytecode_cursor
            );
        }

        Ok(match opc {
            Opcode::CharCount => {
                let len = self.read_u(&mut bytecode_cursor)? as usize;
                if text.len() >= len {
                    Some((len, vec![]))
                } else {
                    None
                }
            }
            Opcode::Literal => {
                let string = self.read_string(&mut bytecode_cursor)?;
                if text.starts_with(string) {
                    Some((string.len(), vec![]))
                } else {
                    None
                }
            }
            Opcode::Range => {
                let chr = if let Some(c) = text.first() {
                    *c
                } else {
                    return Ok(None);
                };
                let string = self.read_string(&mut bytecode_cursor)?;
                if string.len() % 2 != 0 {
                    return Err(self.engine.make_err(
                        "peg/match/range/bad-len",
                        format!(
                            "range needs a string of even length but got {}",
                            string.len()
                        ),
                        Some(Expr::string(string.to_vec())),
                    ));
                }
                let matches = string.chunks_exact(2).any(|r| (r[0]..=r[1]).contains(&chr));
                if matches {
                    Some((1, vec![]))
                } else {
                    None
                }
            }
            Opcode::Set => {
                let chr = if let Some(c) = text.first() {
                    *c
                } else {
                    return Ok(None);
                };
                let string = self.read_string(&mut bytecode_cursor)?;
                let matches = string.contains(&chr);
                if matches {
                    Some((1, vec![]))
                } else {
                    None
                }
            }
            Opcode::Choice => {
                let subrule_count = self.read_byte(&mut bytecode_cursor)?;
                for _ in 0..subrule_count {
                    let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                    let matched = self.match_(ruleptr, full_text, text_cursor)?;
                    if matched.is_some() {
                        // yay!
                        return Ok(matched);
                    }
                }
                // oh no we went through everything and didn't find anything
                None
            }
            Opcode::Sequence => {
                let subrule_count = self.read_byte(&mut bytecode_cursor)?;
                let mut subcursor = 0;
                let mut stack = vec![];
                for _ in 0..subrule_count {
                    let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                    let matched = self.match_(ruleptr, full_text, text_cursor + subcursor)?;
                    let (len, substack) = if let Some(it) = matched {
                        it
                    } else {
                        return Ok(None);
                    };
                    subcursor += len;
                    stack.extend(substack);
                }
                Some((subcursor, stack))
            }
            // All the "match between X and Y of This"
            Opcode::Any
            | Opcode::Some
            | Opcode::Opt
            | Opcode::AtLeast
            | Opcode::AtMost
            | Opcode::Count
            | Opcode::Between => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                let (min, max) = match opc {
                    Opcode::Any => (0, usize::MAX),
                    Opcode::Some => (1, usize::MAX),
                    Opcode::Opt => (0, 1),
                    Opcode::AtLeast | Opcode::AtMost | Opcode::Count => {
                        let n = self.read_u(&mut bytecode_cursor)? as usize;
                        match opc {
                            Opcode::AtLeast => (n, usize::MAX),
                            Opcode::AtMost => (0, n),
                            Opcode::Count => (n, n),
                            _ => unreachable!(),
                        }
                    }
                    Opcode::Between => {
                        let min = self.read_u(&mut bytecode_cursor)?;
                        let max = self.read_u(&mut bytecode_cursor)?;
                        (min as _, max as _)
                    }
                    _ => unreachable!(),
                };
                let mut subcursor = 0;
                let mut stack = vec![];
                for idx in 0.. {
                    let matched = self.match_(ruleptr, full_text, text_cursor + subcursor)?;
                    match matched {
                        Some((len, substack)) => {
                            if idx > max {
                                // oh god we matched too many times!
                                return Ok(None);
                            } else {
                                // ok we're still good
                                subcursor += len;
                                stack.extend(substack);
                            }
                        }
                        None => {
                            return Ok(if idx < min {
                                // oh no we failed and matched too few times
                                None
                            } else {
                                // ok we stopped matching, *and* we're within the ok count!
                                Some((subcursor, stack))
                            });
                        }
                    }
                }
                unreachable!()
            }
            Opcode::Opt => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                match self.match_(ruleptr, full_text, text_cursor)? {
                    Some(it) => Some(it),
                    // and if we failed that's also OK
                    None => Some((0, vec![])),
                }
            }
            Opcode::Capture => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                match self.match_(ruleptr, full_text, text_cursor) {
                    Ok(Some((len, mut stack))) => {
                        let matched = &text[..len];
                        stack.push(Expr::string(matched.to_vec()));
                        Some((len, stack))
                    }
                    ono => return ono,
                }
            }
            Opcode::Replace => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                let replacer = self.read_expr(&mut bytecode_cursor)?.to_owned();
                let matched = self.match_(ruleptr, full_text, text_cursor)?;
                match matched {
                    Some((len, stack)) => {
                        if replacer.is_callable() {
                            let args = Engine::list_to_sexp(&stack);
                            let callee = Expr::pair(replacer, args);
                            let execed = self.engine.eval_inner(self.env.to_owned(), callee)?;
                            Some((len, vec![execed]))
                        } else {
                            Some((len, vec![replacer]))
                        }
                    }
                    None => None,
                }
            }
            Opcode::Group => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                match self.match_(ruleptr, full_text, text_cursor) {
                    Ok(Some((len, stack))) => {
                        let stack_list = Engine::list_to_sexp(&stack);
                        Some((len, vec![stack_list]))
                    }
                    ono => return ono,
                }
            }
            Opcode::If | Opcode::IfNot => {
                let rule_if = self.read_u(&mut bytecode_cursor)? as usize;
                let rule_then = self.read_u(&mut bytecode_cursor)? as usize;
                let matched = self.match_(rule_if, full_text, text_cursor)?;
                match (opc, matched) {
                    (Opcode::If, Some(_)) | (Opcode::IfNot, None) => {
                        self.match_(rule_then, full_text, text_cursor)?
                    }
                    _ => None,
                }
            }
            Opcode::Not => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                match self.match_(ruleptr, full_text, text_cursor)? {
                    Some(_) => None,
                    None => Some((0, vec![])),
                }
            }
            Opcode::Position => Some((0, vec![Expr::integer(text_cursor as _)])),
            Opcode::All => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                match self.match_(ruleptr, full_text, text_cursor)? {
                    Some((len, stack)) if len == text.len() => Some((len, stack)),
                    _ => None,
                }
            }
            Opcode::Jump => {
                let ruleptr = self.read_u(&mut bytecode_cursor)? as usize;
                self.match_(ruleptr, full_text, text_cursor)?
            }
        })
    }

    fn read_u(&mut self, cursor: &mut usize) -> Result<U, Exception> {
        let next_cursor = *cursor + U_SIZE;
        let data = match self.bytecode.get(*cursor..next_cursor) {
            Some(it) => it,
            None => {
                return Err(self.engine.make_err(
                    "peg/match/oob/u",
                    "went out of bounds when reading a u".to_string(),
                    None,
                ))
            }
        };
        let data = data.try_into().unwrap();
        let u = U::from_be_bytes(data);

        if self.debug {
            println!("read u {} at {}", u, cursor);
        }

        *cursor += U_SIZE;
        Ok(u)
    }

    fn read_byte(&mut self, cursor: &mut usize) -> Result<BYTE, Exception> {
        let next_cursor = *cursor + BYTE_SIZE;
        let data = match self.bytecode.get(*cursor..next_cursor) {
            Some(it) => it,
            None => {
                return Err(self.engine.make_err(
                    "peg/match/oob/byte",
                    "went out of bounds when reading a byte".to_string(),
                    None,
                ))
            }
        };
        let data = data.try_into().unwrap();
        let byte = BYTE::from_be_bytes(data);

        if self.debug {
            println!("read byte {} at {}", byte, cursor);
        }

        *cursor += BYTE_SIZE;
        Ok(byte)
    }

    fn read_opcode(&mut self, cursor: &mut usize) -> Result<Opcode, Exception> {
        let byte = self.read_byte(cursor).map_err(|_| {
            self.engine.make_err(
                "peg/match/oob/opcode",
                "went out of bounds when reading an opcode",
                None,
            )
        })?;
        let opc = byte
            .try_into()
            .map_err(|ono: num_enum::TryFromPrimitiveError<_>| {
                self.engine.make_err(
                    "peg/match/bad-opcode",
                    format!("0x{:#X} is not an opcode", ono.number),
                    Some(Expr::integer(ono.number as _)),
                )
            })?;

        Ok(opc)
    }

    fn read_string(&mut self, cursor: &mut usize) -> Result<&'code [u8], Exception> {
        let ptr = self.read_u(cursor)? as usize;
        let len = self.read_u(cursor)? as usize;

        if let Some(s) = self.bytecode.get(ptr..ptr + len) {
            if self.debug {
                println!(
                    "^^^ read string {:?}",
                    s.iter()
                        .cloned()
                        .flat_map(std::ascii::escape_default)
                        .map(|b| b as char)
                        .collect::<String>()
                );
            }
            Ok(s)
        } else {
            Err(self.engine.make_err(
                "peg/match/oob/str",
                "went out of bounds when reading a string from a ptr",
                Some(Engine::list_to_sexp(&[
                    Expr::integer(ptr as _),
                    Expr::integer(len as _),
                ])),
            ))
        }
    }

    fn read_expr<'a>(&'a mut self, cursor: &mut usize) -> Result<&'a Value, Exception> {
        let idx = self.read_byte(cursor)? as usize;
        if let Some(expr) = self.exprs.get(idx) {
            Ok(expr)
        } else {
            Err(self.engine.make_err(
                "peg/match/oob/expr",
                format!(
                    "tried to read an expression at index {} when we only have {}",
                    idx,
                    self.exprs.len()
                ),
                Some(Expr::integer(idx as _)),
            ))
        }
    }
}

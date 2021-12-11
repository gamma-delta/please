use std::{borrow::Cow, collections::HashMap, convert::TryInto};

use gc::{Gc, GcCell};
use itertools::Itertools;

use super::{
    Opcode, BYTE, BYTE_SIZE, EXPR, EXPR_SIZE, OPCODE_SIZE, RULEPTR, RULEPTR_SIZE, STR_SIZE, U,
    U_SIZE,
};
use crate::{
    display::BstrFmt,
    eval::thtd::{check_argc, peg::compression},
    hash::GcMap,
    Engine, EvalResult, Exception, Expr, Namespace,
};

#[allow(clippy::type_complexity)]
const COMPRESSOR: fn(HashMap<usize, Vec<u8>>) -> (Vec<u8>, Vec<(usize, usize)>) =
    compression::reverse_sort;

pub fn compile(engine: &mut Engine, env: Gc<GcCell<Namespace>>, args: &[Gc<Expr>]) -> EvalResult {
    check_argc(engine, args, 1, 1)?;

    let peg = args[0].to_owned();
    let mut builder = Builder {
        bytecode: Vec::new(),
        exprs: Vec::new(),
        lookup: None,
        prev_symbols: HashMap::new(),
        string_slots: HashMap::new(),
        engine,
        env,
    };
    let mut cursor = 0;
    builder.write_rule(&mut cursor, peg)?;

    // Wow, we actually made it, somehow.
    // Deconstruct it back into its parts, and fill in the strings.
    let mut bytecode = builder.bytecode;

    let (buffer, slots) = COMPRESSOR(builder.string_slots);
    for (slot, bufptr) in slots {
        let ptr = bufptr + bytecode.len();

        let uptr: U = if let Ok(u) = ptr.try_into() {
            u
        } else {
            return Err(engine.make_err("peg/compile/too-long/strptr", format!("the bytecode got longer than {} when writing a string and the strptr could not be created", U::MAX), None));
        };
        let dest = &mut bytecode[slot..slot + U_SIZE];
        debug_assert_eq!(
            dest,
            &[0xff, 0xff],
            "tried to overwrite a non-0xFF string slot at {} in the string buffer: {:x?}",
            slot,
            buffer
        );
        dest.copy_from_slice(&uptr.to_be_bytes());
    }

    bytecode.extend(buffer);
    let exprs = Engine::list_to_sexp(&builder.exprs);

    let peg_sym = engine.intern_symbol("peg");

    Ok(Engine::list_to_improper_sexp(
        &[Expr::symbol(peg_sym), Expr::string(bytecode)],
        exprs,
    ))
}

struct Builder<'engine, 'map> {
    bytecode: Vec<u8>,
    exprs: Vec<Gc<Expr>>,

    lookup: Option<&'map GcMap>,
    /// Maps can be used to create recursive rules.
    ///
    /// When we're about to use a symbol to write a rule, we check if that symbol's in here,
    /// which maps symbols to the absolute byte idx of their written rule.
    /// If it is, we just write a `(do <that idx>)`. Otherwise we write that into the map and write the rule.
    ///
    /// This is *not* passed down to further maps, because lower-leveled maps can't call into higher-leveled ones.
    prev_symbols: HashMap<u64, usize>,

    /// Indices of byte idxes we need to write strings to mapped to the expected string.
    string_slots: HashMap<usize, Vec<u8>>,

    engine: &'engine mut Engine,
    env: Gc<GcCell<Namespace>>,
}

impl Builder<'_, '_> {
    /// Write a rule and advance the cursor.
    fn write_rule(&mut self, cursor: &mut usize, peg: Gc<Expr>) -> Result<(), Exception> {
        match &*peg {
            &Expr::Integer(count) => {
                self.reserve(OPCODE_SIZE + U_SIZE);
                self.write_opcode(cursor, Opcode::CharCount);
                self.write_i64(cursor, count)?;
            }
            Expr::String(s) => {
                self.reserve(OPCODE_SIZE + STR_SIZE);
                self.write_opcode(cursor, Opcode::Literal);
                self.write_str(cursor, s)?;
            }
            Expr::Pair(head, tail) => {
                let body = self.engine.sexp_to_list(tail.to_owned())?.ok_or_else(|| {
                    self.engine.make_err(
                        "peg/compile/improper-list",
                        "requires a proper list as the tail of a pattern".to_owned(),
                        Some(tail.to_owned()),
                    )
                })?;
                match **head {
                    Expr::Symbol(s) => {
                        let op = self.engine.get_symbol_str(s).unwrap().to_owned();
                        self.write_rule_from_opcode_and_body(cursor, &op, body, tail.to_owned())?;
                    }
                    Expr::Integer(i) => {
                        // Alias for count: one subexpr argument.
                        let subpeg = if let [subpeg] = body.as_slice() {
                            subpeg.to_owned()
                        } else {
                            return Err(self.engine.make_err(
                                "peg/compile/int-op/argc",
                                "int as an operator (alias for count) requires 1 peg argument"
                                    .to_string(),
                                Some(tail.to_owned()),
                            ));
                        };

                        self.reserve(OPCODE_SIZE + RULEPTR_SIZE + U_SIZE);
                        self.write_opcode(cursor, Opcode::Count);
                        self.write_ruleptr(cursor, subpeg)?;
                        self.write_i64(cursor, i)?;
                    }
                    _ => {
                        return Err(self.engine.make_err(
                            "peg/compile/op-type",
                            format!("cannot use a {} as an operator", head.type_name()),
                            Some(head.to_owned()),
                        ))
                    }
                }
            }
            Expr::Map(new_lookup) => {
                let main_sym = self.engine.intern_symbol("main");
                let main_peg = match new_lookup.get(&Expr::Symbol(main_sym)) {
                    Some(peg) => peg.to_owned(),
                    _ => {
                        return Err(self.engine.make_err(
                            "peg/compile/map/no-main",
                            "a map PEG requires a 'main key as an entry point".to_string(),
                            Some(peg),
                        ))
                    }
                };

                // Get *that* to go recurse on down, and avoid cloning.
                let mut prev_symbols = HashMap::new();
                // for recursively calling main, the main is right here!
                prev_symbols.insert(main_sym, *cursor);
                let mut builder_with_lookup = Builder {
                    bytecode: std::mem::take(&mut self.bytecode),
                    exprs: std::mem::take(&mut self.exprs),
                    lookup: Some(new_lookup),
                    prev_symbols,
                    string_slots: HashMap::new(),

                    engine: self.engine,
                    env: self.env.to_owned(),
                };
                // yes we do actually write the rule here
                builder_with_lookup.write_rule(cursor, main_peg)?;
                // then undestructure it
                self.bytecode = builder_with_lookup.bytecode;
                self.exprs = builder_with_lookup.exprs;
                self.string_slots.extend(builder_with_lookup.string_slots);
            }
            Expr::Symbol(sym) => {
                if let Some(&prev_idx) = self.prev_symbols.get(sym) {
                    // hey we've already used this symbol! let's just jump to it with a `do` opcode.
                    self.reserve(OPCODE_SIZE + RULEPTR_SIZE);
                    self.write_opcode(cursor, Opcode::Jump);
                    // U has the same size as a rule-idx ... we must make sure that always holds.
                    self.write_usize(cursor, prev_idx)?;
                } else {
                    let local_map = self.lookup.map(Cow::Borrowed);
                    let global_map_sym = self.engine.intern_symbol("peg/default-grammar");
                    let global_map = self.env.borrow().lookup(global_map_sym).and_then(|expr| {
                        if let Expr::Map(m) = &*expr {
                            // We MUST clone here :(
                            // this is because otherwise we'd need a lock on the env, and we might need that lock
                            // mutable later.
                            Some(Cow::Owned(m.clone()))
                        } else {
                            None
                        }
                    });
                    for map in local_map.iter().chain(global_map.iter()) {
                        // Do a lookup in the map?
                        if let Some(subpeg) = map.get(&Expr::Symbol(*sym)) {
                            // yoo we found it
                            // Write the index of *this* into the map first, in case that rule calls this.
                            self.prev_symbols.insert(*sym, *cursor);
                            // seamlessly replace the symbol with the rule.
                            return self.write_rule(cursor, subpeg.to_owned());
                        }
                    }

                    return Err(self.engine.make_err(
                        "peg/compile/no-lookup",
                        format!(
                            "could not find a PEG associated with '{}",
                            BstrFmt(self.engine.get_symbol_str(*sym).unwrap())
                        ),
                        Some(peg),
                    ));
                }
            }
            _ => {
                return Err(self.engine.make_err(
                    "peg/compile/bad-matcher",
                    format!("cannot use a {} as a matcher", peg.type_name()),
                    Some(peg),
                ))
            }
        }
        Ok(())
    }

    /// Write a rule given an opcode string and body of the expression.
    ///
    /// Yes this is a dumb name but I call it *once* and I can't think of a better one
    fn write_rule_from_opcode_and_body(
        &mut self,
        cursor: &mut usize,
        op: &[u8],
        body: Vec<Gc<Expr>>,
        tail: Gc<Expr>,
    ) -> Result<(), Exception> {
        match op {
            // Things taking 0 arguments
            b"position" | b"$" => {
                let opc = match op {
                    b"position" | b"$" => Opcode::Position,
                    ono => panic!("i forgor to impl {}", BstrFmt(ono)),
                };
                if !body.is_empty() {
                    return Err(self.engine.make_err(
                        "peg/position/argc",
                        "position takes 0 arguments".to_string(),
                        Some(tail),
                    ));
                }

                self.reserve(OPCODE_SIZE);
                self.write_opcode(cursor, opc);
            }
            // Things taking 1 subpeg argument
            b"any" | b"some" | b"opt" | b"?" | b"capture" | b"<-" | b"quote" | b"group"
            | b"not" | b"!" | b"all" | b"do" => {
                let (opc, real_name) = match op {
                    b"any" => (Opcode::Any, b"any".as_ref()),
                    b"some" => (Opcode::Some, b"some".as_ref()),
                    b"opt" | b"?" => (Opcode::Opt, b"opt".as_ref()),
                    b"capture" | b"<-" | b"quote" => (Opcode::Capture, b"capture".as_ref()),
                    b"group" => (Opcode::Group, b"group".as_ref()),
                    b"not" | b"!" => (Opcode::Not, b"not".as_ref()),
                    b"all" => (Opcode::All, b"all".as_ref()),
                    b"do" => (Opcode::Jump, b"do".as_ref()),
                    ono => panic!("i forgor to impl {}", BstrFmt(ono)),
                };
                let subpeg = if let [subpeg] = body.as_slice() {
                    subpeg.to_owned()
                } else {
                    return Err(self.engine.make_err(
                        &format!("peg/compile/{}/argc", BstrFmt(real_name)),
                        format!("{} requires 1 peg argument", BstrFmt(real_name)),
                        Some(tail),
                    ));
                };

                self.reserve(OPCODE_SIZE + RULEPTR_SIZE);
                self.write_opcode(cursor, opc);
                self.write_ruleptr(cursor, subpeg)?;
            }
            // Things taking 2 subpeg arguments
            b"if" | b"if-not" => {
                let opc = match op {
                    b"if" => Opcode::If,
                    b"if-not" => Opcode::IfNot,
                    ono => panic!("i forgor to impl {}", BstrFmt(ono)),
                };

                let (speg1, speg2) = if let [speg1, speg2] = body.as_slice() {
                    (speg1.to_owned(), speg2.to_owned())
                } else {
                    return Err(self.engine.make_err(
                        &format!("peg/compile/{}/argc", BstrFmt(op)),
                        format!("{} requires 2 peg arguments", BstrFmt(op)),
                        Some(tail),
                    ));
                };

                self.reserve(OPCODE_SIZE + 2 * RULEPTR_SIZE);
                self.write_opcode(cursor, opc);
                self.write_ruleptr(cursor, speg1)?;
                self.write_ruleptr(cursor, speg2)?;
            }

            // Other Stuff
            b"range" => {
                // Concat everything.
                // (range "AZ" "az") -> "AZaz"
                let mut concated = Vec::with_capacity(body.len() * 2);
                for expr in body {
                    let s = match &*expr {
                        Expr::String(s) => s,
                        _ => {
                            return Err(self.engine.make_err(
                                "peg/compile/range/non-string",
                                "range arguments must be 2-byte strings".to_string(),
                                Some(expr),
                            ))
                        }
                    };
                    if s.len() != 2 {
                        return Err(self.engine.make_err(
                            "peg/compile/range/bad-str-len",
                            "range arguments must be 2-byte strings".to_string(),
                            Some(expr),
                        ));
                    }
                    concated.extend(s.iter().copied());
                }

                self.reserve(OPCODE_SIZE + STR_SIZE);
                self.write_opcode(cursor, Opcode::Range);
                self.write_str(cursor, &concated)?;
            }
            b"set" => {
                let set = if let [set] = body.as_slice() {
                    set
                } else {
                    return Err(self.engine.make_err(
                        "peg/compile/set/argc",
                        "set takes one string argument".to_string(),
                        Some(tail),
                    ));
                };
                let mut set = match &**set {
                    Expr::String(s) => s.clone(),
                    _ => {
                        return Err(self.engine.make_err(
                            "peg/compile/set/non-string",
                            "set takes one string argument".to_string(),
                            Some(set.to_owned()),
                        ))
                    }
                };
                set.sort_unstable();
                set.dedup();

                // Do some optimization: perhaps ranges are more optimum?
                // don't bother calculating if the set is too small though
                let mss = minimum_spanning_set(&set);
                let ranges = mss.concat();

                let (opc, arg) = if ranges.len() < set.len() {
                    // well whaddya know
                    (Opcode::Range, ranges)
                } else {
                    // We tried
                    (Opcode::Set, set)
                };

                self.reserve(OPCODE_SIZE + STR_SIZE);
                self.write_opcode(cursor, opc);
                self.write_str(cursor, &arg)?
            }

            b"choice" | b"+" | b"sequence" | b"*" => {
                let count = body.len();
                let count_b: BYTE = if let Ok(b) = count.try_into() {
                    b
                } else {
                    return Err(self.engine.make_err(
                        "peg/compile/too-many-subrules",
                        format!("can only have up to {} subrules", BYTE::MAX),
                        Some(tail),
                    ));
                };

                let opc = if op == b"choice" || op == b"+" {
                    Opcode::Choice
                } else {
                    Opcode::Sequence
                };

                self.reserve(OPCODE_SIZE + BYTE_SIZE + count * RULEPTR_SIZE);
                self.write_opcode(cursor, opc);
                self.write_byte(cursor, count_b);
                for subrule in body {
                    self.write_ruleptr(cursor, subrule)?;
                }
            }

            b"at-least" | b"at-most" | b"count" => {
                let (n, subpeg) = if let [n, subpeg] = body.as_slice() {
                    (n, subpeg.to_owned())
                } else {
                    return Err(self.engine.make_err(
                        &format!("peg/compile/{}/argc", BstrFmt(op)),
                        format!("{} requires one int and one peg argument", BstrFmt(op)),
                        Some(tail),
                    ));
                };
                let n = match **n {
                    Expr::Integer(x) => {
                        if x >= 0 {
                            x
                        } else {
                            return Err(self.engine.make_err(
                                &format!("peg/compile/{}/negative", BstrFmt(op)),
                                format!("{} requires positive int arguments", BstrFmt(op)),
                                Some(n.clone()),
                            ));
                        }
                    }
                    _ => {
                        return Err(self.engine.make_err(
                            &format!("peg/compile/{}/argc", BstrFmt(op)),
                            format!("{} requires one int and one peg argument", BstrFmt(op)),
                            Some(tail),
                        ))
                    }
                };

                let opc = match op {
                    b"at-least" => Opcode::AtLeast,
                    b"at-most" => Opcode::AtMost,
                    b"count" => Opcode::Count,
                    ono => panic!("i forgor to impl {}", BstrFmt(ono)),
                };

                self.reserve(OPCODE_SIZE + RULEPTR_SIZE + U_SIZE);
                self.write_opcode(cursor, opc);
                self.write_ruleptr(cursor, subpeg)?;
                self.write_i64(cursor, n)?;
            }
            b"between" => {
                let (min, max, subpeg) = if let [min, max, subpeg] = body.as_slice() {
                    (min, max, subpeg.to_owned())
                } else {
                    return Err(self.engine.make_err(
                        "peg/compile/between/argc",
                        "between requires 2 int and one peg argument".to_string(),
                        Some(tail),
                    ));
                };
                let min = match **min {
                    Expr::Integer(x) => {
                        if x >= 0 {
                            x
                        } else {
                            return Err(self.engine.make_err(
                                "peg/compile/between/negative",
                                "between requires positive int arguments".to_owned(),
                                Some(min.clone()),
                            ));
                        }
                    }
                    _ => {
                        return Err(self.engine.make_err(
                            "peg/compile/between/argc",
                            "between requires 2 int and one peg argument".to_string(),
                            Some(min.to_owned()),
                        ))
                    }
                };
                let max = match **max {
                    Expr::Integer(x) => {
                        if x >= 0 {
                            x
                        } else {
                            return Err(self.engine.make_err(
                                "peg/compile/between/negative",
                                "between requires positive int arguments".to_owned(),
                                Some(max.clone()),
                            ));
                        }
                    }
                    _ => {
                        return Err(self.engine.make_err(
                            "peg/compile/between/argc",
                            "between requires 2 int and one peg argument".to_string(),
                            Some(max.to_owned()),
                        ))
                    }
                };

                self.reserve(OPCODE_SIZE + RULEPTR_SIZE + 2 * U_SIZE);
                self.write_opcode(cursor, Opcode::Between);
                self.write_ruleptr(cursor, subpeg)?;
                self.write_i64(cursor, min)?;
                self.write_i64(cursor, max)?;
            }
            b"replace" | b"/" => {
                let (subpeg, replacer) = if let [subpeg, subst] = body.as_slice() {
                    (subpeg.to_owned(), subst.to_owned())
                } else {
                    return Err(self.engine.make_err(
                        "peg/replace/argc",
                        "replace takes 1 PEG and 1 any argument".to_string(),
                        Some(tail),
                    ));
                };
                self.reserve(OPCODE_SIZE + RULEPTR_SIZE + EXPR_SIZE);
                self.write_opcode(cursor, Opcode::Replace);
                self.write_ruleptr(cursor, subpeg)?;
                self.write_expr(cursor, replacer)?;
            }

            _ => {
                return Err(self.engine.make_err(
                    "peg/compile/bad-op",
                    format!("did not recognize the operator '{}", BstrFmt(op)),
                    Some(Gc::new(Expr::String(op.to_owned()))),
                ));
            }
        }

        Ok(())
    }

    /// Push this many 0 bytes onto the end of the stack.
    fn reserve(&mut self, size: usize) {
        let len = self.bytecode.len();
        self.bytecode.resize(len + size, 0);
    }

    /// Write a U and move the cursor forward.
    fn write_u(&mut self, cursor: &mut usize, u: U) {
        let end_cursor = *cursor + U_SIZE;
        let slice = &mut self.bytecode[*cursor..end_cursor];
        debug_assert!(
            all_zero(slice),
            "tried to overwrite bytes at {}: {:x?} with {:x}",
            cursor,
            &self.bytecode,
            u,
        );
        slice.copy_from_slice(&u.to_be_bytes());
        *cursor = end_cursor;
    }

    /// Write a byte and move the cursor
    fn write_byte(&mut self, cursor: &mut usize, byte: BYTE) {
        let end_cursor = *cursor + BYTE_SIZE;
        let slice = &mut self.bytecode[*cursor..end_cursor];
        debug_assert!(
            all_zero(slice),
            "tried to overwrite bytes at {}: {:x?} with {:x}",
            cursor,
            self.bytecode,
            byte,
        );
        slice[0] = byte;
        *cursor = end_cursor;
    }

    /// Write an opcode and move the cursor
    fn write_opcode(&mut self, cursor: &mut usize, opc: Opcode) {
        self.write_byte(cursor, opc.into());
    }

    /// Write a usize as a U and move the cursor forward
    fn write_usize(&mut self, cursor: &mut usize, usz: usize) -> Result<(), Exception> {
        let u: U = match usz.try_into() {
            Ok(u) => u,
            Err(_) => {
                return Err(self.engine.make_err(
                    "peg/compile/overlarge-usize",
                    format!(
                        "the usize {} was too big to put into a {}-byte uint",
                        usz, U_SIZE
                    ),
                    Some(Expr::integer(usz as _)),
                ))
            }
        };
        self.write_u(cursor, u);
        Ok(())
    }

    /// Write an i64 as a U and move the cursor forward
    fn write_i64(&mut self, cursor: &mut usize, i: i64) -> Result<(), Exception> {
        let u: U = match i.try_into() {
            Ok(u) => u,
            Err(_) => {
                return Err(self.engine.make_err(
                    "peg/compile/overlarge-i64",
                    format!("the i64 {} cannot be put into a {}-byte uint", i, U_SIZE),
                    Some(Expr::integer(i)),
                ))
            }
        };
        self.write_u(cursor, u);
        Ok(())
    }

    /// Pretend to write a string. Save the string to `string_slots`, write the length, and write `0xffff` as the pointer.
    fn write_str(&mut self, cursor: &mut usize, string: &[u8]) -> Result<(), Exception> {
        let end_cursor = *cursor + STR_SIZE;
        let slice = &mut self.bytecode[*cursor..end_cursor];
        debug_assert!(all_zero(slice), "tried to overwrite bytes at {}", cursor);

        let str_len = string.len();
        let _prev = self.string_slots.insert(*cursor, string.to_owned());
        debug_assert!(_prev.is_none());

        // Now tell the caller about the "ptr" and len
        self.write_u(cursor, U::MAX);
        self.write_usize(cursor, str_len).map_err(|_| {
            self.engine.make_err(
                "peg/compile/too-long/string",
                format!(
                    "the string had a length of {}, longer than the allowed {}",
                    string.len(),
                    RULEPTR::MAX
                ),
                Some(Expr::integer(string.len() as _)),
            )
        })?;
        Ok(())
    }
    /// Append the rule to the bytecode and put the pointer at the cursor.
    fn write_ruleptr(&mut self, cursor: &mut usize, rule: Gc<Expr>) -> Result<(), Exception> {
        // Pointer to where the rule goes, aka the end.
        let rule_ptr = self.bytecode.len();
        // Write that rule there
        let mut subcursor = rule_ptr;
        self.write_rule(&mut subcursor, rule.to_owned())?;
        // And write the pointer to *this* cursor.
        self.write_usize(cursor, rule_ptr).map_err(|_| {
            self.engine.make_err(
                "peg/compile/too-long/ruleptr",
                format!(
                    "bytecode got longer than {} and the ruleptr could not be created",
                    RULEPTR::MAX
                ),
                Some(rule),
            )
        })
    }

    /// Write an expr ptr and save the expr to the builder.
    fn write_expr(&mut self, cursor: &mut usize, expr: Gc<Expr>) -> Result<(), Exception> {
        let expr_ptr = if let Some(already_idx) = self.exprs.iter().position(|other| &expr == other)
        {
            // hey we know this already
            already_idx
        } else {
            let ptr = self.exprs.len();
            self.exprs.push(expr);
            ptr
        };
        let b = if let Ok(b) = expr_ptr.try_into() {
            b
        } else {
            return Err(self.engine.make_err(
                "peg/compile/too-long/exprs",
                format!("cannot have more than {} exprs", EXPR::MAX),
                Some(Expr::integer(expr_ptr as _)),
            ));
        };
        self.write_byte(cursor, b);
        Ok(())
    }
}

fn all_zero(slice: &[u8]) -> bool {
    slice.iter().all(|b| *b == 0)
}

/// Input must already be sorted and deduped.
fn minimum_spanning_set(set: &[u8]) -> Vec<[u8; 2]> {
    if set.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    // Byte idx we're forming the set *from*
    let mut pivot_point = 0;
    for (idx, &byte) in set.iter().enumerate() {
        let pivot = set[pivot_point];
        // Assuming it counts up one-by-one, (idx - pivot_point) should have the same span as (byte - pivot).
        // We check if
        //   1. the current byte is more than the pivot
        //   2. the distance from the pivot to the byte == the distance from the pivot *point* to the current idx.
        if Some((idx - pivot_point) as u8) != byte.checked_sub(pivot) {
            // oh no we failed at this point!
            // the *last* iteration was good, so save that.
            // this will never OOB because the distance for both will be 0 on the first iteration
            let prev_byte = set[idx - 1];
            out.push([pivot, prev_byte]);
            // set this as the new pivot
            pivot_point = idx;
        }
    }
    // And we do need to cap it off.
    let pivot = set[pivot_point];
    let last = *set.last().unwrap(); // ok because we checked the set is not empty.
    out.push([pivot, last]);
    out
}

#[test]
fn mss() {
    use itertools::Itertools;

    for (test, ans) in [
        (b"abcde".as_ref(), b"ae".as_ref()),
        (b"ABCDEFGabcdefg", b"AGag"),
        (b"testing 12345", b"15  steeiinngg"),
    ] {
        let mut test = test.to_vec();
        test.sort_unstable();
        test.dedup();
        let mut ans = ans.chunks_exact(2).map(|x| [x[0], x[1]]).collect_vec();
        ans.sort_unstable();

        let mut mss = minimum_spanning_set(&test);
        mss.sort_unstable();
        assert_eq!(mss, ans);
    }
}

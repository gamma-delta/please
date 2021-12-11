//! Print and write exprs.

use std::fmt::{Debug, Display};

use super::*;

impl Engine {
    /// Write an expression to a string. Reading this string
    /// should yield the same as the original (with some caveats for procedures &c).
    pub fn write_expr(&mut self, expr: Gc<Expr>) -> Result<String, Exception> {
        self.reify(&expr)?;
        fn recur(
            engine: &mut Engine,
            w: &mut impl Write,
            expr: Gc<Expr>,
        ) -> Result<(), fmt::Error> {
            match &*expr {
                Expr::Integer(i) => write!(w, "{}", i),
                Expr::Float(f) => write!(w, "{:?}", f),
                Expr::Symbol(sym) => {
                    if let Some(s) = engine.get_symbol_str(*sym) {
                        write!(w, "{}", BstrFmt(s))
                    } else {
                        write!(w, "<unknown #{}>", sym)
                    }
                }
                Expr::Bool(b) => {
                    write!(w, "{}", b)
                }
                Expr::Pair(..) | Expr::LazyPair(..) => {
                    let (car, cdr) = engine.split_cons(expr).unwrap();
                    fn write_list<W: Write>(
                        engine: &mut Engine,
                        w: &mut W,
                        car: Gc<Expr>,
                        cdr: Gc<Expr>,
                    ) -> Result<(), fmt::Error> {
                        recur(engine, w, car)?;
                        match &*cdr {
                            // Proper list's end, do nothing
                            Expr::Nil => Ok(()),
                            // Proper list, leave space for the next thing.
                            Expr::Pair(..) | Expr::LazyPair(..) => {
                                let (cdar, cddr) = engine.split_cons(cdr).unwrap();
                                write!(w, " ")?;
                                write_list(engine, w, cdar, cddr)
                            }
                            // Just a pair
                            _ => {
                                write!(w, " . ")?;
                                recur(engine, w, cdr)
                            }
                        }
                    }

                    write!(w, "(")?;
                    write_list(engine, w, car, cdr)?;
                    write!(w, ")")
                }
                Expr::Nil => {
                    write!(w, "()")
                }
                Expr::String(s) => {
                    write!(w, "{:?}", BstrFmt(s))
                }
                Expr::SpecialForm { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<special form {}>", BstrFmt(name))
                    } else {
                        write!(w, "<anonymous special form>")
                    }
                }
                Expr::NativeProcedure { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<native proc {}>", BstrFmt(name))
                    } else {
                        write!(w, "<anonymous native proc>")
                    }
                }
                Expr::Procedure {
                    arg_spec,
                    body,
                    env,
                    ..
                } => {
                    write!(w, "(")?;
                    if env.is_some() {
                        write!(w, "lambda ")?;
                    } else {
                        write!(w, "macro ")?;
                    }

                    recur(engine, w, arg_spec.to_owned())?;

                    for body_expr in body {
                        write!(w, " ")?;
                        recur(engine, w, body_expr.clone())?;
                    }

                    write!(w, ")")
                }
                Expr::Map(m) => {
                    write!(w, "#{{")?;
                    for (idx, (k, v)) in m.iter().enumerate() {
                        recur(engine, w, k.to_owned())?;
                        write!(w, " ")?;
                        recur(engine, w, v.to_owned())?;
                        if idx != m.len() - 1 {
                            write!(w, " ")?;
                        }
                    }
                    write!(w, "}}")
                }
                Expr::Transient(t) => {
                    let lock = t.borrow();
                    match &*lock {
                        Some(t) => {
                            write!(w, "(transient/new ")?;
                            let waaugh = Gc::new((**t).clone());
                            recur(engine, w, waaugh)?;
                            write!(w, ")")
                        }
                        None => write!(w, "<taken-transient>"),
                    }
                }
            }
        }
        let mut writer = String::new();
        recur(self, &mut writer, expr).unwrap();
        Ok(writer)
    }

    /// Print the expression to a string
    /// in a nice and human-readable way.
    pub fn print_expr(&mut self, expr: Gc<Expr>) -> Result<String, Exception> {
        self.reify(&expr)?;
        fn recur<W: Write>(
            engine: &mut Engine,
            w: &mut W,
            expr: Gc<Expr>,
        ) -> Result<(), fmt::Error> {
            match &*expr {
                Expr::Integer(i) => write!(w, "{}", i),
                Expr::Float(f) => write!(w, "{:?}", f),
                Expr::String(s) => {
                    write!(w, "{}", BstrFmt(s))
                }
                Expr::Bool(b) => {
                    write!(w, "{}", b)
                }
                Expr::Symbol(sym) => {
                    if let Some(s) = engine.get_symbol_str(*sym) {
                        write!(w, "{}", BstrFmt(s))
                    } else {
                        write!(w, "<unknown #{}>", sym)
                    }
                }
                Expr::Pair(..) | Expr::LazyPair(..) => {
                    let (car, cdr) = engine.split_cons(expr).unwrap();
                    fn write_list<W: Write>(
                        engine: &mut Engine,
                        w: &mut W,
                        car: Gc<Expr>,
                        cdr: Gc<Expr>,
                    ) -> Result<(), fmt::Error> {
                        recur(engine, w, car)?;
                        match &*cdr {
                            // Proper list's end, do nothing
                            Expr::Nil => Ok(()),
                            // Proper list, leave space for the next thing.
                            Expr::Pair(..) | Expr::LazyPair(..) => {
                                let (cdar, cddr) = engine.split_cons(cdr).unwrap();
                                write!(w, " ")?;
                                write_list(engine, w, cdar, cddr)
                            }
                            // Just a pair
                            _ => {
                                write!(w, " . ")?;
                                recur(engine, w, cdr.to_owned())
                            }
                        }
                    }

                    write!(w, "(")?;
                    write_list(engine, w, car, cdr)?;
                    write!(w, ")")
                }
                Expr::Nil => {
                    write!(w, "()")
                }
                Expr::SpecialForm { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<native func {}>", BstrFmt(name))
                    } else {
                        write!(w, "<anonymous native func>")
                    }
                }
                Expr::NativeProcedure { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<native proc {}>", BstrFmt(name))
                    } else {
                        write!(w, "<anonymous native proc>")
                    }
                }
                Expr::Procedure { .. } => {
                    write!(w, "<procedure>")
                }
                Expr::Map(m) => {
                    write!(w, "#{{")?;
                    for (idx, (k, v)) in m.iter().enumerate() {
                        recur(engine, w, k.to_owned())?;
                        write!(w, " ")?;
                        recur(engine, w, v.to_owned())?;
                        if idx != m.len() - 1 {
                            write!(w, " ")?;
                        }
                    }
                    write!(w, "}}")
                }
                Expr::Transient(t) => {
                    let lock = t.borrow();
                    match &*lock {
                        Some(t) => {
                            write!(w, "(transient/new ")?;
                            let waaugh = Gc::new((**t).clone());
                            recur(engine, w, waaugh)?;
                            write!(w, ")")
                        }
                        None => write!(w, "<taken-transient>"),
                    }
                }
            }
        }
        let mut writer = String::new();
        recur(self, &mut writer, expr).unwrap();
        Ok(writer)
    }
}

pub struct BstrFmt<B: AsRef<[u8]>>(pub B);

impl<B: AsRef<[u8]>> Display for BstrFmt<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for &b in self.0.as_ref() {
            match b {
                0x20..=0x7e => f.write_char(b as char)?,
                // pad it to a length of 2 with 0s if need be
                ono => write!(f, "\\x{:02X}", ono)?,
            }
        }
        Ok(())
    }
}

impl<B: AsRef<[u8]>> Debug for BstrFmt<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"")?;
        for &b in self.0.as_ref() {
            match b {
                b'\t' => write!(f, "\\t")?,
                b'\r' => write!(f, "\\r")?,
                b'\n' => write!(f, "\\n")?,
                b'\"' => write!(f, "\\\"")?,
                b'\x0b' => write!(f, "\\f")?,
                b'\x0c' => write!(f, "\\v")?,
                b'\x07' => write!(f, "\\a")?,
                b'\0' => write!(f, "\\0")?,
                0x20..=0x7e => f.write_char(b as char)?,
                // pad it to a length of 2 with 0s if need be
                ono => write!(f, "\\x{:02X}", ono)?,
            }
        }
        write!(f, "\"")
    }
}

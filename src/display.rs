//! Print and write exprs.

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
                Expr::Float(f) => write!(w, "{}", f),
                Expr::Symbol(sym) => {
                    if let Some(s) = engine.get_symbol_str(*sym) {
                        write!(w, "{}", s)
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
                    write!(w, "{:?}", String::from_utf8_lossy(s))
                }
                Expr::SpecialForm { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<special form {}>", name)
                    } else {
                        write!(w, "<anonymous special form>")
                    }
                }
                Expr::NativeProcedure { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<native proc {}>", name)
                    } else {
                        write!(w, "<anonymous native proc>")
                    }
                }
                Expr::Procedure {
                    args,
                    body,
                    variadic,
                    env,
                    ..
                } => {
                    write!(w, "(")?;
                    if env.is_some() {
                        write!(w, "lambda (")?;
                    } else {
                        write!(w, "macro (")?;
                    }

                    let (draw_now_args, special) = if *variadic && args.last().is_some() {
                        (&args[..args.len() - 1], true)
                    } else {
                        (args.as_slice(), false)
                    };

                    for (idx, (sym, default)) in draw_now_args.iter().enumerate() {
                        let symbol = engine.get_symbol_str(*sym).unwrap_or("<unknown>");
                        if let Some(default) = default {
                            write!(w, "[{} ", symbol)?;
                            recur(engine, w, default.to_owned())?;
                            write!(w, "]")?;
                        } else {
                            write!(w, "{}", symbol)?;
                        }
                        if idx != draw_now_args.len() - 1 {
                            write!(w, " ")?;
                        }
                    }
                    if special {
                        // a little hacky but we can't have default trail args
                        let last = engine
                            .get_symbol_str(args.last().unwrap().0)
                            .unwrap_or("<unknown>");
                        write!(w, ". {}", last)?;
                    }
                    write!(w, ")")?;

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
                Expr::Float(f) => write!(w, "{}", f),
                Expr::String(s) => {
                    write!(w, "{}", String::from_utf8_lossy(s))
                }
                Expr::Symbol(sym) => {
                    if let Some(s) = engine.get_symbol_str(*sym) {
                        write!(w, "{}", s)
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
                        write!(w, "<native func {}>", name)
                    } else {
                        write!(w, "<anonymous native func>")
                    }
                }
                Expr::NativeProcedure { name, .. } => {
                    if let Some(name) = engine.get_symbol_str(*name) {
                        write!(w, "<native proc {}>", name)
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
            }
        }
        let mut writer = String::new();
        recur(self, &mut writer, expr).unwrap();
        Ok(writer)
    }
}

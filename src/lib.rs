mod eval;
mod parse;

use eval::TailRec;
use itertools::Itertools;
pub use parse::{ExprParseError, ExprParseErrorInfo};

use std::{
    collections::HashMap,
    fmt::{self, Write},
};

#[macro_use]
extern crate gc;
#[macro_use]
extern crate derivative;

use bimap::BiHashMap;
use gc::{Finalize, Gc, GcCell, Trace};

#[derive(Derivative, Trace, Finalize)]
#[derivative(Debug)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    String(String),
    /// Interned string.
    ///
    /// This number is the ID of this symbol and is used for quick equality
    /// and looking up the original string.
    Symbol(Symbol),
    /// Pointer to two elements.
    Pair(Gc<Expr>, Gc<Expr>),
    LazyPair(LazyExprCell, LazyExprCell, Gc<GcCell<Namespace>>),
    /// Lack of a value
    Nil,

    /// Named native special "function" like define, and the symbol of its name.
    SpecialForm {
        #[derivative(Debug(format_with = "Expr::form_formatter"))]
        #[unsafe_ignore_trace]
        func: fn(&mut Engine, Gc<GcCell<Namespace>>, &[Gc<Expr>]) -> Result<TailRec, Exception>,
        name: Symbol,
    },
    /// Named native function and the symbol of its name.
    NativeProcedure {
        #[derivative(Debug(format_with = "Expr::func_formatter"))]
        #[unsafe_ignore_trace]
        func: fn(&mut Engine, Gc<GcCell<Namespace>>, &[Gc<Expr>]) -> EvalResult,
        name: Symbol,
    },

    Procedure {
        args: Vec<(Symbol, Option<Gc<Expr>>)>,
        body: Vec<Gc<Expr>>,
        /// This is None iff the body's a macro
        env: Option<Gc<GcCell<Namespace>>>,
        variadic: bool,
        /// Possible name of the `define` that created this.
        name: Option<Symbol>,
    },
}

impl Expr {
    #[allow(clippy::type_complexity)]
    fn form_formatter(
        _: &fn(&mut Engine, Gc<GcCell<Namespace>>, &[Gc<Expr>]) -> Result<TailRec, Exception>,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "fn(...)")
    }
    #[allow(clippy::type_complexity)]
    fn func_formatter(
        _: &fn(&mut Engine, Gc<GcCell<Namespace>>, &[Gc<Expr>]) -> EvalResult,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "fn(...)")
    }
}

/// Execution state and reader.
#[derive(Debug, Clone)]
pub struct Engine {
    /// Map of all known interned symbols to their handles, and vice versa
    interned_symbols: BiHashMap<String, Symbol>,
    /// Number of symbols that have ever been created
    akashic_symbol_count: u64,

    /// Standard library, aka top level namespace
    thtdlib: Gc<GcCell<Namespace>>,
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine {
    pub fn new() -> Self {
        let mut out = Self {
            interned_symbols: BiHashMap::new(),
            akashic_symbol_count: 0,
            thtdlib: Gc::new(GcCell::new(Namespace {
                mappings: HashMap::new(),
                parent: None,
            })),
        };
        eval::add_thtandard_library(&mut out);
        out
    }

    /// Reads the source and return one token from it.
    pub fn read_one(&mut self, s: &str, source_name: String) -> Result<Expr, ExprParseError> {
        parse::read_one(s, source_name, self)
    }

    /// Reads the source and returns everything found in it.
    pub fn read_many(&mut self, s: &str, source_name: String) -> Result<Vec<Expr>, ExprParseError> {
        parse::read_many(s, source_name, self)
    }

    /// Read and eval everything in the source file, returning
    /// the item in tail position (or `()` if there isn't anything).
    pub fn read_eval(&mut self, s: &str, source_name: String) -> Result<Gc<Expr>, ExprParseError> {
        Ok(self
            .read_many(s, source_name)?
            .into_iter()
            .map(|e| self.eval(self.thtdlib(), Gc::new(e)))
            .last()
            .unwrap_or_else(|| Gc::new(Expr::Nil)))
    }

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
                    write!(w, "{:?}", s)
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

                    if *variadic {}

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
                    write!(w, "{}", s)
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
            }
        }
        let mut writer = String::new();
        recur(self, &mut writer, expr).unwrap();
        Ok(writer)
    }

    /// Make or get the symbol handle of the symbol represented by the given string.
    pub fn intern_symbol(&mut self, sym: &str) -> Symbol {
        if let Some(already) = self.interned_symbols.get_by_left(sym) {
            *already
        } else {
            let id = self.akashic_symbol_count;
            self.interned_symbols.insert(sym.to_string(), id);

            self.akashic_symbol_count += 1;
            id
        }
    }

    /// Create a symbol guaranteed to not have been seen before.
    /// Even if some overly-clever person tries to make a symbol with the same
    /// string content, it won't work, because this symbol will have an internal
    /// ID different from it.
    pub fn unique_symbol(&mut self) -> Symbol {
        let sym = format!("_uniq#{}", self.akashic_symbol_count);
        self.intern_symbol(&sym)
    }

    /// Get the ID of the already-existing symbol with the given name.
    pub fn find_symbol(&self, sym: &str) -> Option<Symbol> {
        self.interned_symbols.get_by_left(sym).copied()
    }

    pub fn get_symbol_str(&self, symbol_id: Symbol) -> Option<&str> {
        if let Some(sym) = self.interned_symbols.get_by_right(&symbol_id) {
            Some(sym.as_str())
        } else {
            None
        }
    }

    /// Turn a cons list into a vector of indices.
    /// If the given index or any cdr doesn't point to a `Pair`
    /// or `Null` (ie it's not a proper list)u
    /// then `None` is returned.
    pub fn sexp_to_list(&mut self, expr: Gc<Expr>) -> Result<Option<Vec<Gc<Expr>>>, Exception> {
        let (list, end) = self.expr_to_improper_list(expr)?;
        Ok(if let Expr::Nil = &*end {
            Some(list)
        } else {
            None
        })
    }

    /// Eval a LazyExprCell.
    pub fn eval_cell(&mut self, env: &Gc<GcCell<Namespace>>, expr: &LazyExprCell) -> EvalResult {
        let (expr, done) = &mut *expr.borrow_mut();
        if *done {
            Ok(expr.clone())
        } else {
            *done = true;
            let evaluated = self.eval_inner(env.clone(), expr.clone());
            *expr = match &evaluated {
                Ok(val) => val.clone(),
                Err(_) => Gc::new(Expr::Nil),
            };
            evaluated
        }
    }

    /// Split a cons pair into head and tail
    pub fn car(&mut self, expr: Gc<Expr>) -> EvalResult {
        match &*expr {
            Expr::Pair(car, _) => Ok(car.to_owned()),
            Expr::LazyPair(car, _, ctx) => self.eval_cell(ctx, car),
            _ => Err(crate::eval::thtd::bad_arg_type(self, expr, 0, "pair")),
        }
    }
    pub fn cdr(&mut self, expr: Gc<Expr>) -> EvalResult {
        match &*expr {
            Expr::Pair(_, cdr) => Ok(cdr.to_owned()),
            Expr::LazyPair(_, cdr, ctx) => self.eval_cell(ctx, cdr),
            _ => Err(crate::eval::thtd::bad_arg_type(self, expr, 0, "pair")),
        }
    }
    pub fn split_cons_verb(
        &mut self,
        expr: Gc<Expr>,
    ) -> Result<Option<(Gc<Expr>, Gc<Expr>)>, Exception> {
        let val = match &*expr {
            Expr::Pair(car, cdr) => (car.to_owned(), cdr.to_owned()),
            Expr::LazyPair(car, cdr, ctx) => (self.eval_cell(ctx, car)?, self.eval_cell(ctx, cdr)?),
            _ => return Ok(None),
        };
        Ok(Some(val))
    }
    pub fn split_cons(&mut self, expr: Gc<Expr>) -> Result<(Gc<Expr>, Gc<Expr>), Exception> {
        match self.split_cons_verb(expr.clone())? {
            Some(pair) => Ok(pair),
            None => Err(crate::eval::thtd::bad_arg_type(self, expr, 0, "pair")),
        }
    }
    /// Reify an expr by evaluating all contained LazyExprCells.
    pub fn reify(&mut self, expr: &Gc<Expr>) -> Result<(), Exception> {
        if let Some((car, cdr)) = &self.split_cons_verb(expr.clone())? {
            self.reify(car)?;
            self.reify(cdr)?;
        }
        Ok(())
    }

    /// Turn an improper list into the list leading up to the last element,
    /// and the last element. Proper lists will have the last element be `()`.
    pub fn expr_to_improper_list(
        &mut self,
        mut expr: Gc<Expr>,
    ) -> Result<(Vec<Gc<Expr>>, Gc<Expr>), Exception> {
        let mut out = vec![];
        while let Some((car, cdr)) = self.split_cons_verb(expr.clone())? {
            out.push(car);
            expr = cdr;
        }
        Ok((out, expr))
    }

    /// Create a cons list from the given list, and return its head.
    pub fn list_to_sexp(list: &[Gc<Expr>]) -> Gc<Expr> {
        if let Some((car, cdr)) = list.split_first() {
            Gc::new(Expr::Pair(car.clone(), Self::list_to_sexp(cdr)))
        } else {
            Gc::new(Expr::Nil)
        }
    }

    pub fn is_truthy(&self, expr: Gc<Expr>) -> bool {
        match &*expr {
            Expr::Nil => false,
            Expr::Symbol(sym) => {
                let f = self.interned_symbols.get_by_left("false");
                if let Some(f) = f {
                    // If it equals "false" return false
                    // Otherwise true
                    f != sym
                } else {
                    // somehow undefined false
                    true
                }
            }
            // everything else is truthy
            _ => true,
        }
    }

    pub fn make_bool(&mut self, b: bool) -> Gc<Expr> {
        Gc::new(Expr::Symbol(self.intern_symbol(if b {
            "true"
        } else {
            "false"
        })))
    }

    /// Make an error, a cons list `'(! "msg")` or `'(! "msg" userdata)`.
    /// As a helper, if `userdata` is None, the userdata becomes `()`.
    ///
    /// This is given a trace of `[]` to start; `eval_inner` pushes sources to it as needed.
    pub fn make_err(&mut self, name: &str, msg: String, userdata: Option<Gc<Expr>>) -> Exception {
        let sym = self.intern_symbol(name);

        Exception {
            id: sym,
            info: msg,
            data: userdata.unwrap_or_else(|| Gc::new(Expr::Nil)),
            call_trace: EvalSource { trace: Vec::new() },
        }
    }

    /// Get a reference to the engine's thtdlib.
    pub fn thtdlib(&self) -> Gc<GcCell<Namespace>> {
        self.thtdlib.clone()
    }
}

/// Mapping of symbols to places in memory.
#[derive(Debug, Clone, Trace, Finalize)]
pub struct Namespace {
    mappings: HashMap<Symbol, Gc<Expr>>,
    parent: Option<Gc<GcCell<Namespace>>>,
}

impl Namespace {
    pub fn new(parent: Gc<GcCell<Namespace>>) -> Self {
        Self {
            mappings: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn insert(&mut self, symbol: Symbol, target: Gc<Expr>) {
        self.mappings.insert(symbol, target);
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<Gc<Expr>> {
        self.mappings.get(&symbol).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().lookup(symbol))
        })
    }
}

#[derive(Debug)]
pub struct Exception {
    /// Name of the exception
    pub id: Symbol,
    /// User-readable information
    pub info: String,
    /// Additional associated data
    pub data: Value,
    /// Call stack
    pub call_trace: EvalSource,
}

impl Exception {
    pub fn into_expr(self, engine: &mut Engine) -> Value {
        let symbols = self
            .call_trace
            .trace
            .into_iter()
            .map(|s| {
                Gc::new(if let Some(s) = s {
                    Expr::Symbol(s)
                } else {
                    Expr::Nil
                })
            })
            .collect_vec();
        let trace = Engine::list_to_sexp(&symbols);

        Engine::list_to_sexp(&[
            Gc::new(Expr::Symbol(engine.intern_symbol("!"))),
            Gc::new(Expr::Symbol(self.id)),
            Gc::new(Expr::String(self.info)),
            trace,
            self.data,
        ])
    }
}

pub type Symbol = u64;
/// Normal values
pub type Value = Gc<Expr>;

/// Result of any calculation that may throw an exception
pub type EvalResult = Result<Value, Exception>;

//#[derive(Debug, Trace, Finalize)]
pub type LazyExprCell = GcCell<(Gc<Expr>, bool)>;

/// Where was an expr evaled from?
#[derive(Debug)]
pub struct EvalSource {
    /// An empty vec means the top level.
    /// A `[H... | Entry]` means it was called from the function/macro with the given name.
    /// (Lambdas/inline macros are None).
    ///
    /// In other words, for each level down in the call stack another thing is pushed to the trace,
    /// and the deepest call is to the left.
    pub trace: Vec<Option<Symbol>>,
}

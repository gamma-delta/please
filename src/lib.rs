mod display;
mod eval;
mod hasheq;
mod lazy;
mod parse;
mod repl;

use eval::TailRec;
use hasheq::HashEqExpr;
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
        func: NativeFn<TailRec>,
        name: Symbol,
    },
    /// Named native function and the symbol of its name.
    NativeProcedure {
        #[derivative(Debug(format_with = "Expr::func_formatter"))]
        #[unsafe_ignore_trace]
        func: Result<NativeFn<Value>, NativeFn<TailRec>>,
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

    Map(HashMap<HashEqExpr, Gc<Expr>>),
}

impl Expr {
    #[allow(clippy::type_complexity)]
    fn form_formatter(
        _: &NativeFn<TailRec>,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "fn(...)")
    }
    #[allow(clippy::type_complexity)]
    fn func_formatter(
        _: &Result<NativeFn<Value>, NativeFn<TailRec>>,
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
    pub fn read_eval(
        &mut self,
        s: &str,
        source_name: String,
    ) -> Result<Result<Gc<Expr>, Exception>, ExprParseError> {
        Ok(self
            .read_many(s, source_name)?
            .into_iter()
            .map(|e| self.eval_inner(self.thtdlib(), Gc::new(e)))
            .find_or_last(Result::is_err)
            .unwrap_or_else(|| Ok(Gc::new(Expr::Nil))))
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
        Engine::list_to_improper_sexp(list, Gc::new(Expr::Nil))
    }

    /// Create a cons list from the given list, and return its head.
    pub fn list_to_improper_sexp(mut list: &[Gc<Expr>], mut last: Gc<Expr>) -> Gc<Expr> {
        while let [head @ .., tail] = list {
            list = head;
            last = Gc::new(Expr::Pair(tail.clone(), last));
        }
        last
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

#[derive(Debug, Clone)]
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

type NativeFn<T> = fn(&mut Engine, Gc<GcCell<Namespace>>, &[Gc<Expr>]) -> Result<T, Exception>;

/// Where was an expr evaled from?
#[derive(Debug, Clone)]
pub struct EvalSource {
    /// An empty vec means the top level.
    /// A `[H... | Entry]` means it was called from the function/macro with the given name.
    /// (Lambdas/inline macros are None).
    ///
    /// In other words, for each level down in the call stack another thing is pushed to the trace,
    /// and the deepest call is to the left.
    pub trace: Vec<Option<Symbol>>,
}

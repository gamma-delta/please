mod display;
mod eval;
mod hash;
mod lazy;
mod parse;
mod repl;
mod type_predicates;

use hash::GcMap;
pub use parse::{ExprParseError, ExprParseErrorInfo};

use eval::TailRec;
use itertools::Itertools;

use std::{
    borrow::Borrow,
    cmp::{Eq, PartialEq},
    collections::HashMap,
    fmt::{self, Write},
    hash::Hash,
};

#[macro_use]
extern crate gc;
#[macro_use]
extern crate derivative;

use bimap::BiHashMap;
use gc::{Finalize, Gc, GcCell, Trace};

#[derive(Derivative, Trace, Finalize, Clone)]
#[derivative(Debug)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Bool(bool),
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
        arg_spec: Gc<Expr>,
        body: Vec<Gc<Expr>>,
        /// This is None iff the body's a macro
        env: Option<Gc<GcCell<Namespace>>>,
        /// Possible name of the `define` that created this.
        name: Option<Symbol>,
    },

    Map(GcMap),

    Transient(GcCell<Option<Box<Expr>>>),
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

    pub fn integer(i: i64) -> Gc<Self> {
        Gc::new(Self::Integer(i))
    }

    pub fn float(f: f64) -> Gc<Self> {
        Gc::new(Self::Float(f))
    }

    pub fn string<S: Into<Vec<u8>>>(s: S) -> Gc<Self> {
        Gc::new(Self::String(s.into()))
    }

    pub fn bool(b: bool) -> Gc<Self> {
        Gc::new(Self::Bool(b))
    }

    pub fn symbol(s: Symbol) -> Gc<Self> {
        Gc::new(Self::Symbol(s))
    }

    pub fn pair(car: Gc<Expr>, cdr: Gc<Expr>) -> Gc<Self> {
        Gc::new(Self::Pair(car, cdr))
    }

    pub fn nil() -> Gc<Self> {
        Gc::new(Self::Nil)
    }

    pub fn map(m: GcMap) -> Gc<Self> {
        Gc::new(Self::Map(m))
    }

    pub fn transient(expr: Expr) -> Gc<Self> {
        Gc::new(Expr::Transient(GcCell::new(Some(Box::new(expr)))))
    }
}

/// Because NaN was a mistake, all NaN are considered equal to each other.
/// I don't care what the IEEE says. Shut up.
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => {
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            (Bool(a), Bool(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Nil, Nil) => true,
            (Pair(ahead, atail), Pair(bhead, btail)) => ahead == bhead && atail == btail,
            (SpecialForm { func: a, .. }, SpecialForm { func: b, .. }) => std::ptr::eq(a, b),
            (NativeProcedure { func: a, .. }, NativeProcedure { func: b, .. }) => {
                std::ptr::eq(a, b)
            }
            (
                Procedure {
                    arg_spec: a_args,
                    body: a_body,
                    env: a_env,
                    ..
                },
                Procedure {
                    arg_spec: b_args,
                    body: b_body,
                    env: b_env,
                    ..
                },
            ) => a_args == b_args && a_body == b_body && a_env.is_some() == b_env.is_some(),
            (Map(a), Map(b)) => a == b,

            (LazyPair(..), LazyPair(..)) => std::ptr::eq(self, other),
            (Transient(..), Transient(..)) => std::ptr::eq(self, other),
            _ => false,
        }
    }
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Expr::*;
        std::mem::discriminant(self).hash(state);

        match self {
            Integer(x) => state.write_i64(*x),
            Float(x) => state.write_u64(if x.is_nan() {
                // nansbad!
                0x6e616e7362616421
            } else {
                x.to_bits()
            }),
            String(x) => x.hash(state),
            Symbol(x) => state.write_u64(*x),
            Bool(b) => b.hash(state),
            Nil => {}
            Pair(head, tail) => {
                head.hash(state);
                tail.hash(state);
            }
            LazyPair(..) => std::ptr::hash(self, state),
            SpecialForm { func, .. } => std::ptr::hash(func, state),
            NativeProcedure { func, .. } => std::ptr::hash(func, state),
            Procedure {
                arg_spec: args,
                body,
                env,
                ..
            } => {
                args.hash(state);
                body.hash(state);
                env.is_some().hash(state);
            }
            Map(map) => map.hash(state),
            Transient(..) => std::ptr::hash(self, state),
        }
    }
}

/// Execution state and reader.
#[derive(Debug, Clone)]
pub struct Engine {
    /// Map of all known interned symbols to their handles, and vice versa
    interned_symbols: BiHashMap<Vec<u8>, Symbol>,
    /// Number of symbols that have ever been created
    akashic_symbol_count: u64,

    /// Standard library, aka top level namespace
    thtdlib: Gc<GcCell<Namespace>>,

    /// If this is Some, we're recording profiling information.
    /// Maps symbols to how many times we've evaled them and the total number of seconds we've been executing it form
    profiler: Option<HashMap<u64, (u64, f64)>>,
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
            profiler: None,
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
    pub fn intern_symbol<B: AsRef<[u8]>>(&mut self, sym: B) -> Symbol {
        let sym = sym.as_ref();
        if let Some(already) = self.interned_symbols.get_by_left(sym) {
            *already
        } else {
            let id = self.akashic_symbol_count;
            self.interned_symbols.insert(sym.to_vec(), id);

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
    pub fn find_symbol<B: AsRef<[u8]>>(&self, sym: B) -> Option<Symbol> {
        self.interned_symbols.get_by_left(sym.as_ref()).copied()
    }

    pub fn get_symbol_str(&self, symbol_id: Symbol) -> Option<&[u8]> {
        if let Some(sym) = self.interned_symbols.get_by_right(&symbol_id) {
            Some(sym.as_slice())
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
            Expr::Bool(b) => *b,
            // everything else is truthy
            _ => true,
        }
    }

    pub fn make_bool(&mut self, b: bool) -> Gc<Expr> {
        Expr::bool(b)
    }

    /// Make an error, a cons list `'(! "msg")` or `'(! "msg" userdata)`.
    /// As a helper, if `userdata` is None, the userdata becomes `()`.
    ///
    /// This is given a trace of `[]` to start; `eval_inner` pushes sources to it as needed.
    pub fn make_err<S1, S2>(&mut self, name: S1, msg: S2, userdata: Option<Gc<Expr>>) -> Exception
    where
        S1: AsRef<str>,
        S2: AsRef<str>,
    {
        let sym = self.intern_symbol(name.as_ref());

        Exception {
            id: sym,
            info: msg.as_ref().to_string(),
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
    /// The `parent` is required because there should be exactly one namespace with no parents,
    /// the root, and it should only be constructable inside the module.
    pub fn new(parent: Gc<GcCell<Namespace>>) -> Self {
        Self::new_with(parent, HashMap::new())
    }

    pub fn new_with(parent: Gc<GcCell<Namespace>>, mappings: HashMap<Symbol, Gc<Expr>>) -> Self {
        Self {
            mappings,
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
                .and_then(|parent| parent.as_ref().borrow().lookup(symbol))
        })
    }

    pub fn merge_from(&mut self, others: HashMap<Symbol, Gc<Expr>>) {
        self.mappings.extend(others);
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
            Gc::new(Expr::String(self.info.into_bytes())),
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

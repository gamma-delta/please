mod eval;
mod parse;

use std::{
    collections::HashMap,
    fmt::{self, Write},
};

use nonempty::NonEmpty;
pub use parse::{ExprParseError, ExprParseErrorInfo};

use bimap::BiHashMap;
#[macro_use]
extern crate derivative;
use generational_arena::{Arena, Index};

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum Expr {
    Integer(i64),
    /// Interned string.
    ///
    /// This number is the ID of this symbol and is used for quick equality
    /// and looking up the original string.
    Symbol(u64),
    /// Pointer to two elements.
    Pair(Index, Index),
    /// Lack of a value
    Nil,

    String(String),

    /// Named native function call, and the symbol of their name
    NativeFunction {
        #[derivative(Debug(format_with = "Expr::func_formatter"))]
        func: fn(&mut Engine, &mut Namespace, &[Index]) -> Expr,
        name: u64,
    },
}

impl Expr {
    fn func_formatter(
        _: &fn(&mut Engine, &mut Namespace, &[Index]) -> Expr,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "fn(&mut Engine, &mut Namespace, &[Index]) -> Expr")
    }
}

/// Execution state and reader.
#[derive(Debug, Clone)]
pub struct Engine {
    /// All data not immediately accessible in an execution.
    /// This is the heap to an execution's stack.
    heap: Arena<Expr>,

    /// Map of all known interned symbols to their handles, and vice versa
    interned_symbols: BiHashMap<String, u64>,
    /// Where on the heap symbols are.
    symbol_poses: HashMap<u64, Index>,
    /// Number of symbols that have ever been created
    akashic_symbol_count: u64,

    /// Standard library, aka top level namespace
    thtdlib: HashMap<u64, Index>,
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine {
    pub fn new() -> Self {
        let mut out = Self {
            heap: Arena::new(),
            interned_symbols: BiHashMap::new(),
            symbol_poses: HashMap::new(),
            akashic_symbol_count: 0,
            thtdlib: HashMap::new(),
        };
        eval::add_thtandard_library(&mut out);
        out
    }

    /// Add an expression to the heap.
    pub fn insert(&mut self, expr: Expr) -> Index {
        self.heap.insert(expr)
    }

    /// Get an expression from the heap.
    pub fn get(&self, index: Index) -> &Expr {
        self.heap.get(index).unwrap()
    }

    /// Get an expression from the heap.
    pub fn get_mut(&mut self, index: Index) -> &mut Expr {
        self.heap.get_mut(index).unwrap()
    }

    /// Make or get the symbol handle of the symbol represented by the given string.
    pub fn intern_symbol(&mut self, sym: &str) -> u64 {
        if let Some(already) = self.interned_symbols.get_by_left(sym) {
            *already
        } else {
            let id = self.akashic_symbol_count;
            let handle = self.heap.insert(Expr::Symbol(id));
            self.symbol_poses.insert(id, handle);
            self.interned_symbols.insert(sym.to_string(), id);

            self.akashic_symbol_count += 1;
            id
        }
    }

    /// Find the index of the symbol. Panics if it doesn't exist
    /// (because it should always be put on the heap somewhere.)
    pub fn find_symbol(&self, symbol_id: u64) -> Index {
        *self.symbol_poses.get(&symbol_id).unwrap()
    }

    pub fn read_source(&mut self, s: &str) -> Result<Expr, ExprParseError> {
        parse::read_entire(s, self)
    }

    pub fn print_expr(&self, idx: Index) -> String {
        let mut writer = String::new();
        self.write_expr(&mut writer, self.get(idx)).unwrap();
        writer
    }

    fn write_expr<'a>(&'a self, w: &mut impl Write, expr: &'a Expr) -> Result<(), fmt::Error> {
        match expr {
            Expr::Integer(i) => write!(w, "{}", i),
            Expr::Symbol(sym) => {
                if let Some(s) = self.get_symbol_str(*sym) {
                    write!(w, "{}", s)
                } else {
                    write!(w, "<unknown #{}>", sym)
                }
            }
            Expr::Pair(car, cdr) => {
                write!(w, "(")?;
                self.write_expr(w, self.get(*car))?;
                write!(w, " ")?;
                self.write_expr(w, self.get(*cdr))?;
                write!(w, ")")
            }
            Expr::Nil => {
                write!(w, "()")
            }
            Expr::String(s) => {
                write!(w, "{:?}", s)
            }
            Expr::NativeFunction { name, .. } => {
                if let Some(name) = self.get_symbol_str(*name) {
                    write!(w, "<native func {}>", name)
                } else {
                    write!(w, "<anonymous native func>")
                }
            }
        }
    }

    pub fn get_symbol_str(&self, symbol_id: u64) -> Option<&str> {
        if let Some(sym) = self.interned_symbols.get_by_right(&symbol_id) {
            Some(sym.as_str())
        } else {
            None
        }
    }

    /// Return the expr associated with the symbol in
    /// the given namespace or the top-level namespace.
    ///
    /// If this returns Some, it is safe to immediately use it to [`get`]
    /// an Expr.
    ///
    /// [`get`]: Engine::get
    pub fn lookup_symbol(&self, symbol_id: u64, env: &Namespace) -> Option<Index> {
        // NonEmpty doesn't impl DoubleEnded wauugh
        (0..env.mappings.len())
            .rev()
            .find_map(|idx| {
                let map = &env.mappings[idx];
                map.get(&symbol_id).copied()
            })
            .or_else(|| self.thtdlib.get(&symbol_id).copied())
    }

    /// Get the index of the (possibly newly interned) symbol.
    pub fn get_symbol_index(&mut self, symbol: &str) -> Index {
        let id = self.intern_symbol(symbol);
        self.find_symbol(id)
    }

    /// Get the index of a symbol without interning.
    pub fn get_existing_symbol_index(&self, symbol: &str) -> Option<Index> {
        let id = self.interned_symbols.get_by_left(symbol)?;
        self.symbol_poses.get(id).copied()
    }

    /// Turn a cons list into a vector of indices.
    /// If the given index or any cdr doesn't point to a `Pair`
    /// or `Null` (ie it's not a proper list)
    /// then `None` is returned.
    pub fn sexp_to_vector(&self, idx: Index) -> Option<Vec<Index>> {
        let elt = self.get(idx);
        let (car, cdr) = match elt {
            Expr::Pair(car, cdr) => (car, cdr),
            // finish iterating
            Expr::Nil => return Some(Vec::new()),
            _ => {
                println!("not proper list: {}", self.print_expr(idx));
                return None;
            }
        };
        let mut out = vec![*car];
        out.extend(self.sexp_to_vector(*cdr)?);
        Some(out)
    }

    pub fn is_truthy(&self, idx: Index) -> bool {
        let data = self.get(idx);
        match data {
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
}

/// Mapping of symbols to places in memory.
///
/// Starts completely empty.
#[derive(Clone)]
pub struct Namespace {
    mappings: NonEmpty<HashMap<u64, Index>>,
}

impl Default for Namespace {
    fn default() -> Self {
        Self::new()
    }
}

impl Namespace {
    pub fn new() -> Self {
        Self {
            mappings: NonEmpty::new(HashMap::new()),
        }
    }

    pub fn add(&mut self, symbol: u64, target: Index) {
        self.mappings.last_mut().insert(symbol, target);
    }

    pub fn push(&mut self) {
        self.mappings.push(HashMap::new())
    }

    pub fn pop(&mut self) -> Option<HashMap<u64, Index>> {
        self.mappings.pop()
    }
}

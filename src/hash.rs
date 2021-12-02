//! Wrapper around a HashableHashMap so it's garbage collectible.

use gc::{Finalize, Gc, Trace};
use hashable::HashableHashMap;

use crate::Expr;

#[derive(PartialEq, Eq, Hash, Clone, Derivative)]
#[derivative(Debug = "transparent")]
pub struct GcMap(HashableHashMap<Gc<Expr>, Gc<Expr>>);

impl GcMap {
    pub fn new() -> GcMap {
        GcMap(HashableHashMap::new())
    }
}

impl Finalize for GcMap {}
unsafe impl Trace for GcMap {
    custom_trace!(this, {
        for (k, v) in this.0.iter() {
            mark(k);
            mark(v);
        }
    });
}

impl std::ops::Deref for GcMap {
    type Target = HashableHashMap<Gc<Expr>, Gc<Expr>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for GcMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for GcMap {
    fn default() -> Self {
        Self::new()
    }
}

impl From<HashableHashMap<Gc<Expr>, Gc<Expr>>> for GcMap {
    fn from(it: HashableHashMap<Gc<Expr>, Gc<Expr>>) -> Self {
        Self(it)
    }
}

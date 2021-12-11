//! # Parsing Expression Grammars
//!
//! It's like [Janet's](https://janet-lang.org/docs/peg.html), but worse!
//!
//! ## Bytecode
//!
//! Because I enjoy pain, let's also try to compile it, why not.
//!
//! A full PEG is a list, with the first element being the symbol `'peg`, the second element
//! a string for the *bytecode* and the remainder being a list of *Value*s.
//!
//! **Bytecode** is a list of one or more *rules*, possibly interspersed with *string*s.
//!
//! **Rules** are a single byte *opcode*, followed by 0 or more *arguments*.
//!
//! **Arguments** are all typed. Each argument type is the same length.
//!
//! - `u`: A 2-byte signed integer
//! - `byte`: A 1-byte unsigned int
//! - `str`: A string. `u` for the absolute position in the bytecode of the string, and `u` for its len.
//! - `rule`: A `u` for the absolute position in the bytecode of *another* rule.
//! - `expr`: 1 byte representing the index of the expression in the cdr of the top-level list.
//!
//! **Strings** are just bags of bytes. The length is stored with the ptr, and there is no terminating \0
//! or similar.
//!
//! Numbers are always written and read big-endian.
//!
//! ## Writing Bytecode
//!
//! Integers and ops not taking any rules/strings are simply written to the bytecode one-by-one.
//!
//! Because we know exactly how long each rule is when written to bytecode,
//! we can write new rules onto the end of the bytecode.
//! When writing a rule that calls other rules or needs a string, we reserve enough space for the
//! current rule, then push the subrule/string to the end of the bytecode.
//!
//! Maps write the `main` rule. Then, when we recursively write *that* rule's subrules,
//! we know to look up symbols in that context map.
mod compile;
mod compression;
mod execute;

pub use compile::compile;
pub use execute::match_;

use num_enum::{IntoPrimitive, TryFromPrimitive};

type U = u16;
type RULEPTR = U;
type EXPR = u8;
type BYTE = u8;

const U_SIZE: usize = std::mem::size_of::<U>();
const RULEPTR_SIZE: usize = std::mem::size_of::<RULEPTR>();
const EXPR_SIZE: usize = std::mem::size_of::<EXPR>();
const BYTE_SIZE: usize = std::mem::size_of::<BYTE>();

/// One `u` for the pointer, one `u` for the len.
const STR_SIZE: usize = 2 * U_SIZE;

/// Size of the opcode
const OPCODE_SIZE: usize = std::mem::size_of::<Opcode>();

/// Opcode bytes.
///
/// Opcodes start at 1 so 0 bytes can be detected as an error, *hopefully*
#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum Opcode {
    // === Literals ===
    /// Match X chars.
    ///
    /// `u`
    CharCount = 1,
    /// Match this literal string.
    ///
    /// `str`
    Literal,
    /// Match these ranges. The string needs to have an even length.
    ///
    /// `str`
    Range,
    /// Match this set of characters.
    ///
    /// `str`
    Set,

    // === Seqs ===
    /// Try matching each expression in sequence.
    ///
    /// One `byte` to show how many rules to store, then that many ruleptrs.
    /// `byte rule...`
    Choice,
    /// Match all exprs one-by-one
    ///
    /// One `byte` to show how many rules to store, then that many ruleptrs.
    /// `byte rule...`
    Sequence,

    // === Counting ===
    /// Match 0+ of the second argument.
    ///
    /// `rule`
    Any,
    /// Match 1+ of the second argument.
    ///
    /// `rule`
    Some,
    /// Match 0 or 1 of this.
    ///
    /// `rule`
    Opt,
    /// Match at least this many of the first argument.
    ///
    /// `rule u`
    AtLeast,
    /// Match at most this many of the first argument.
    ///
    /// `rule u`
    AtMost,
    /// Match exactly this many of the first argument.
    ///
    /// `rule u`
    Count,
    /// Match between second and third of the first argument.
    /// Note this is NOT THE SAME ORDER that `(between min max)` is written in the PEG!
    ///
    /// `rule u u`
    Between,

    // === Captures ===
    /// Take the matched text and stick it on the capture stack.
    ///
    /// `rule`
    Capture,
    /// Take the matched text, and run the expression given on it.
    ///
    /// `rule expr`
    Replace,
    /// Run the given PEG, then stick all of *its* captures on the capture stack.
    ///
    /// `rule`
    Group,

    // === Conditionals ===
    /// If the first PEG matches, return the second's matches.
    ///
    /// `rule rule`
    If,
    /// If the first PEG does *not* match, return the second's matches.
    ///
    /// `rule rule`
    IfNot,
    /// Match 0 chars if the given expr does *not* match.
    ///
    /// `rule`
    Not,

    // === Misc ===
    /// Push the current byte position onto the capture stack and match 0 chars.
    ///
    /// `<no args>`
    Position,
    /// If the given PEG does not match the entirely of the remaining text, this does not match.
    ///
    /// `rule`
    All,
    /// Jump to the inner rule. This exists mostly as a method of indirection, to help with recursive PEGs.
    /// (If this ever is exposed to the user for some reason it will be as "Do" or something like that)
    ///
    /// `rule`
    Jump,
}

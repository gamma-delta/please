//! Because I don't know how to let a point go: a lot of string compression algorithms for
//! writing the bag of literal strings at the end.
//!
//! All of these functions take a map that maps string slot indices to strings,
//! and returns a completed string buffer and a mapping of those string slot indices to positions in the buffer.
//! Note you must then add the length of the bytecode to get the *real* pointers.

use std::collections::HashMap;

use itertools::Itertools;

pub fn naive(string_slots: HashMap<usize, Vec<u8>>) -> (Vec<u8>, Vec<(usize, usize)>) {
    let mut buffer = Vec::new();
    let out = string_slots
        .into_iter()
        .map(|(slot, string)| {
            let ptr = buffer.len();
            buffer.extend(string);
            (slot, ptr)
        })
        .collect();
    (buffer, out)
}

pub fn reverse_sort(string_slots: HashMap<usize, Vec<u8>>) -> (Vec<u8>, Vec<(usize, usize)>) {
    let mut strings = string_slots.into_iter().collect_vec();
    // It would be cool to do some kind of minimum superstring thing but that's NP-hard.
    // Let's instead naively:
    // - sort by len, longest first
    // - if the string exists in a buffer, use that
    // - else, append the string to the buffer and use that.
    // it's like an interned string pool but a little better.
    strings.sort_unstable_by(|a, b| a.1.len().cmp(&b.1.len()).reverse());
    let mut buffer = Vec::new();
    let out = strings
        .into_iter()
        .map(|(slot, string)| {
            let len = string.len();
            let bufptr = if len == 0 {
                // then it doesn't really matter where you put it does it
                0
            } else {
                let already_idx = buffer
                    .windows(string.len())
                    .position(|slice| slice == string);
                if let Some(already_idx) = already_idx {
                    already_idx
                } else {
                    // Welp we tried
                    let ptr = buffer.len();
                    buffer.extend(string.iter().copied());
                    ptr
                }
            };
            (slot, bufptr)
        })
        .collect();
    (buffer, out)
}

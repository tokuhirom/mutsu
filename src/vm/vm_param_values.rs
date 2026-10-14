//! The bound parameters of one fast method call, kept inline.
//!
//! `call_compiled_method_fast_in_unit` collects `(name, symbol, value)` for each bound
//! parameter before it builds the callee's env. A `Vec` of them allocates on the first push,
//! so every method call with an argument paid a malloc/free pair (~250 Ir of the 1.2x gap
//! between `$o.m($x)` and `$o.m()`, #10959). Almost every method binds one or two parameters,
//! so those live in the struct and only a longer list spills to the heap.

use crate::symbol::Symbol;
use crate::value::Value;

/// A bound parameter: the name (matched against the body's locals), its interned symbol (the
/// env key) and the bound value.
pub(super) type ParamValue<'a> = (&'a str, Symbol, Value);

const INLINE: usize = 2;

pub(super) struct ParamValues<'a> {
    inline: [Option<ParamValue<'a>>; INLINE],
    len: usize,
    spill: Vec<ParamValue<'a>>,
}

impl<'a> ParamValues<'a> {
    pub(super) fn new() -> Self {
        ParamValues {
            inline: [None, None],
            len: 0,
            spill: Vec::new(),
        }
    }

    // Cost: O(1) amortized; allocates only past `INLINE` entries.
    pub(super) fn push(&mut self, entry: ParamValue<'a>) {
        if self.len < INLINE {
            self.inline[self.len] = Some(entry);
        } else {
            self.spill.push(entry);
        }
        self.len += 1;
    }

    pub(super) fn len(&self) -> usize {
        self.len
    }

    // Cost: O(n), n = bound parameters.
    pub(super) fn iter(&self) -> impl Iterator<Item = &ParamValue<'a>> {
        self.inline.iter().flatten().chain(self.spill.iter())
    }
}

impl<'a> std::ops::Index<usize> for ParamValues<'a> {
    type Output = ParamValue<'a>;

    fn index(&self, i: usize) -> &ParamValue<'a> {
        assert!(i < self.len, "ParamValues index {i} out of range {}", self.len);
        match i.checked_sub(INLINE) {
            None => self.inline[i]
                .as_ref()
                .expect("an index below `len` and `INLINE` is filled"),
            Some(rest) => &self.spill[rest],
        }
    }
}

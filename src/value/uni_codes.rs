//! The codepoint store of a `Uni` (#8673).
//!
//! rakudo declares `Uni` as `is repr('VMArray') is array_type(uint32)`, and
//! nqp code treats one as exactly that: it consumes a `Uni` with
//! `nqp::shift_i`, builds one with `nqp::push_i`, indexes it with
//! `nqp::atpos_i` and rewrites it with `nqp::splice` (JSON::Fast's string
//! scanner and its escaper). So the store is what MoarVM keeps: a packed
//! `uint32` buffer with a start offset, which makes a front shift O(1).
//!
//! It used to be an `Array` of boxed `Value`s. Every codepoint then cost a
//! NaN-box encode on the way in, a decode and a drop on the way out, and the
//! general `ArrayData` editor's bookkeeping (hole bitmap, native backing) on
//! both. JSON::Fast's `unjsonify-string` does one shift and one push per
//! character of every non-word string it decodes, which made that loop the
//! largest single cost of a decode.
//!
//! The store sits in its own `Gc` node, so two clones of one `Uni` share it,
//! the way two references to a VMArray do. It holds no `Value`, so it has no
//! `Gc` edge and is never a cycle candidate.

use crate::gc::{ErasedGc, Trace};

/// The capacity a store grows to on its first [`UniCodes::push`].
const PUSH_START_CAPACITY: usize = 32;

/// A `Uni`'s codepoints: `buf[head..]` is live. The dead prefix left by
/// [`UniCodes::shift`] is dropped once it outgrows the live part, so it never
/// holds more slots than the store has live.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct UniCodes {
    buf: Vec<u32>,
    head: usize,
}

impl UniCodes {
    // Cost: O(1).
    pub fn from_vec(buf: Vec<u32>) -> Self {
        UniCodes { buf, head: 0 }
    }

    /// The live codepoints.
    // Cost: O(1).
    pub fn as_slice(&self) -> &[u32] {
        &self.buf[self.head..]
    }

    // Cost: O(1).
    pub fn len(&self) -> usize {
        self.buf.len() - self.head
    }

    // Cost: O(1).
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // Cost: O(1).
    pub fn get(&self, idx: usize) -> Option<u32> {
        self.as_slice().get(idx).copied()
    }

    /// Append one codepoint (`nqp::push_i`).
    // Cost: O(1) amortized.
    #[inline]
    pub fn push(&mut self, code: u32) {
        if self.buf.len() == self.buf.capacity() {
            self.grow_for_push();
        }
        self.buf.push(code);
    }

    /// Make room for at least one more codepoint. A store built by pushing
    /// starts at [`PUSH_START_CAPACITY`] slots instead of `Vec`'s four: a
    /// `Uni` filled one `push_i` at a time (JSON::Fast's `unjsonify-string`
    /// copies every character of an escaped string this way) otherwise paid
    /// a reallocation at 4, 8, 16 and 32 codepoints.
    // Cost: O(e) to move the e elements into the larger buffer.
    #[cold]
    #[inline(never)]
    fn grow_for_push(&mut self) {
        let extra = self.buf.capacity().max(PUSH_START_CAPACITY);
        self.buf.reserve(extra);
    }

    /// Remove the last codepoint (`nqp::pop_i`).
    // Cost: O(1).
    pub fn pop(&mut self) -> Option<u32> {
        if self.is_empty() {
            return None;
        }
        let code = self.buf.pop();
        if self.head == self.buf.len() {
            self.clear();
        }
        code
    }

    /// Remove the first codepoint (`nqp::shift_i`): the start offset advances,
    /// as MoarVM's VMArray does.
    // Cost: O(1) amortized: the dead prefix is compacted only once it is
    // larger than the live part, so each slot is moved at most once.
    #[inline]
    pub fn shift(&mut self) -> Option<u32> {
        let code = *self.buf.get(self.head)?;
        self.head += 1;
        let live = self.buf.len() - self.head;
        if live == 0 {
            self.clear();
        } else if self.head > live {
            self.compact();
        }
        Some(code)
    }

    /// Insert one codepoint at the front (`nqp::unshift_i`). The dead prefix a
    /// shift left behind is reused as front slack.
    // Cost: O(1) when a shift left a dead slot, else O(e), e = elements.
    pub fn unshift(&mut self, code: u32) {
        if self.head > 0 {
            self.head -= 1;
            self.buf[self.head] = code;
        } else {
            self.buf.insert(0, code);
        }
    }

    /// Store `code` at `idx`, growing the store with zeros when `idx` is past
    /// the end (`nqp::bindpos_i`).
    // Cost: O(1); O(i - e) when growing, i = index, e = elements.
    pub fn set(&mut self, idx: usize, code: u32) {
        let at = self.head + idx;
        if at >= self.buf.len() {
            self.buf.resize(at + 1, 0);
        }
        self.buf[at] = code;
    }

    /// Resize to `n` codepoints, growing with zeros (`nqp::setelems`).
    // Cost: O(|n - e|) amortized, e = elements.
    pub fn resize(&mut self, n: usize) {
        self.buf.resize(self.head + n, 0);
    }

    /// Remove every codepoint.
    // Cost: O(1).
    pub fn clear(&mut self) {
        self.buf.clear();
        self.head = 0;
    }

    /// Replace the contents with `codes`.
    // Cost: O(n), n = new codepoints.
    pub fn replace(&mut self, codes: impl IntoIterator<Item = u32>) {
        self.clear();
        self.buf.extend(codes);
    }

    /// Replace the contents with the codepoints of `text`, in one allocation:
    /// `str::chars` reports a quarter of the byte length as its lower size
    /// bound, so collecting it grew the buffer several times over.
    // Cost: O(n), n = bytes of `text`.
    pub fn replace_with_str(&mut self, text: &str) {
        self.clear();
        if text.is_ascii() {
            self.buf.extend(text.bytes().map(u32::from));
        } else {
            self.buf.reserve(text.len());
            // Codepoints ARE the contents here. str-prim: allow
            self.buf.extend(text.chars().map(u32::from));
        }
    }

    // Cost: O(e), e = live elements.
    fn compact(&mut self) {
        if self.head > 0 {
            self.buf.drain(..self.head);
            self.head = 0;
        }
    }
}

impl Trace for UniCodes {
    // A packed codepoint buffer holds no `Value`, so it has no `Gc` child.
    fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}

    fn currently_acyclic(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::UniCodes;

    #[test]
    fn shift_advances_the_start_and_compacts_lazily() {
        let mut c = UniCodes::from_vec((0..10).collect());
        for want in 0..6 {
            assert_eq!(c.shift(), Some(want));
        }
        assert_eq!(c.as_slice(), &[6, 7, 8, 9]);
        c.push(10);
        assert_eq!(c.len(), 5);
        assert_eq!(c.get(0), Some(6));
        while c.shift().is_some() {}
        assert!(c.is_empty());
        assert_eq!(c.shift(), None);
    }

    #[test]
    fn set_grows_with_zeros_and_unshift_reuses_the_offset() {
        let mut c = UniCodes::from_vec(vec![1, 2, 3]);
        c.shift();
        c.set(4, 9);
        assert_eq!(c.as_slice(), &[2, 3, 0, 0, 9]);
        assert_eq!(c.pop(), Some(9));
        c.unshift(7);
        assert_eq!(c.as_slice(), &[7, 2, 3, 0, 0]);
        c.unshift(6);
        assert_eq!(c.as_slice(), &[6, 7, 2, 3, 0, 0]);
        c.resize(2);
        assert_eq!(c.as_slice(), &[6, 7]);
    }

    #[test]
    fn replace_with_str_takes_codepoints_in_one_allocation() {
        let mut c = UniCodes::from_vec(vec![1, 2]);
        c.shift();
        c.replace_with_str("a/b");
        assert_eq!(c.as_slice(), &[97, 47, 98]);
        c.replace_with_str("\u{E9}\u{1F600}x");
        assert_eq!(c.as_slice(), &[0xE9, 0x1F600, 0x78]);
        let long = "https://example.org/".repeat(8);
        c.replace_with_str(&long);
        assert_eq!(c.len(), long.len());
        assert!(c.buf.capacity() >= long.len());
    }

    #[test]
    fn push_onto_an_empty_store_reserves_a_starting_capacity() {
        let mut c = UniCodes::default();
        c.push(1);
        assert!(c.buf.capacity() >= super::PUSH_START_CAPACITY);
        for i in 2..=100 {
            c.push(i);
        }
        assert_eq!(c.len(), 100);
        assert_eq!(c.get(99), Some(100));
    }
}

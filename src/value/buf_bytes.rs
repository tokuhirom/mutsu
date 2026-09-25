//! [`BufBytes`] — a buffer node's contiguous storage, with O(1) amortized
//! removal and insertion at the **front** as well as the back.
//!
//! A `Buf` used as a queue (`nqp::shift_i` / `nqp::unshift` in a loop) on a
//! plain `Vec<u8>` memmoves every remaining byte on each call, O(e). MoarVM's
//! `VMArray` keeps a `start` offset in its body instead, so both ends are O(1)
//! amortized (#9191). `BufBytes` does the same: the live bytes are
//! `vec[head..]`, and the dead prefix `vec[..head]` doubles as front slack.
//!
//! ADR-0015's contract is unchanged: the live bytes are contiguous, and
//! [`as_ptr`](slice::as_ptr) (through `Deref`) points at element 0 — the
//! address the REPR body block and a C caller see. A front edit may move that
//! address, just as a reallocating append may; neither survives MoarVM either.

use std::ops::{Deref, DerefMut};

/// The live bytes are `vec[head..]`. `head` never exceeds `vec.len()`.
pub(crate) struct BufBytes {
    vec: Vec<u8>,
    head: usize,
}

/// Dead prefixes shorter than this are never compacted away on their own: a
/// short queue is cheaper to leave alone than to keep memmoving.
const MIN_COMPACT: usize = 64;

impl BufBytes {
    /// No bytes.
    pub(crate) fn new() -> BufBytes {
        BufBytes {
            vec: Vec::new(),
            head: 0,
        }
    }

    /// The bytes the live region could grow to without reallocating.
    pub(crate) fn capacity(&self) -> usize {
        self.vec.capacity() - self.head
    }

    /// Drop every byte, keeping the allocation.
    pub(crate) fn clear(&mut self) {
        self.vec.clear();
        self.head = 0;
    }

    /// Append `bytes` at the back.
    pub(crate) fn extend_from_slice(&mut self, bytes: &[u8]) {
        self.vec.extend_from_slice(bytes);
    }

    /// Keep only the first `len` live bytes.
    pub(crate) fn truncate(&mut self, len: usize) {
        self.vec.truncate(self.head + len);
    }

    /// Grow or shrink the live region to `len` bytes, filling with `value`.
    pub(crate) fn resize(&mut self, len: usize, value: u8) {
        self.vec.resize(self.head + len, value);
    }

    /// Replace the live bytes `start..end` with `with`.
    // Cost: O(t + k), t = live bytes after `end`, k = bytes inserted.
    pub(crate) fn splice(&mut self, start: usize, end: usize, with: &[u8]) {
        let (s, e) = (self.head + start, self.head + end);
        self.vec.splice(s..e, with.iter().copied());
    }

    /// Replace every byte with `bytes`, reusing the allocation.
    pub(crate) fn set(&mut self, bytes: &[u8]) {
        self.clear();
        self.vec.extend_from_slice(bytes);
    }

    /// Remove the first `n` live bytes (at most all of them).
    // Cost: O(1) amortized. The dead prefix is compacted away only once it is
    // at least as long as the live bytes, so each compaction's memmove is paid
    // for by the removals that built the prefix.
    pub(crate) fn drop_front(&mut self, n: usize) {
        self.head = (self.head + n).min(self.vec.len());
        if self.head == self.vec.len() {
            self.clear();
        } else if self.head >= MIN_COMPACT && self.head >= self.vec.len() - self.head {
            self.vec.drain(..self.head);
            self.head = 0;
        }
    }

    /// Insert `bytes` before the first live byte.
    // Cost: O(k) amortized, k = bytes inserted. Out of front slack, the
    // storage is rebuilt with slack as large as the live bytes, so the copy is
    // paid for by the insertions that will use that slack up.
    pub(crate) fn insert_front(&mut self, bytes: &[u8]) {
        let k = bytes.len();
        if self.head < k {
            let live = self.vec.len() - self.head;
            let slack = k.max(live).max(MIN_COMPACT);
            let mut vec = Vec::with_capacity(slack + live);
            vec.resize(slack, 0);
            vec.extend_from_slice(&self.vec[self.head..]);
            self.vec = vec;
            self.head = slack;
        }
        self.head -= k;
        self.vec[self.head..self.head + k].copy_from_slice(bytes);
    }

    /// The live bytes as an owned `Vec`, without a copy when there is no dead
    /// prefix.
    pub(crate) fn into_vec(mut self) -> Vec<u8> {
        if self.head > 0 {
            self.vec.drain(..self.head);
        }
        self.vec
    }
}

impl Default for BufBytes {
    fn default() -> BufBytes {
        BufBytes::new()
    }
}

impl From<Vec<u8>> for BufBytes {
    fn from(vec: Vec<u8>) -> BufBytes {
        BufBytes { vec, head: 0 }
    }
}

impl Deref for BufBytes {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.vec[self.head..]
    }
}

impl DerefMut for BufBytes {
    fn deref_mut(&mut self) -> &mut [u8] {
        &mut self.vec[self.head..]
    }
}

impl Clone for BufBytes {
    /// A clone holds only the live bytes.
    fn clone(&self) -> BufBytes {
        BufBytes::from(self.to_vec())
    }
}

impl PartialEq for BufBytes {
    fn eq(&self, other: &BufBytes) -> bool {
        **self == **other
    }
}

impl std::fmt::Debug for BufBytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn front_and_back_edits_read_back_in_order() {
        let mut b = BufBytes::from(vec![1, 2, 3]);
        b.drop_front(1);
        assert_eq!(&*b, &[2, 3]);
        b.insert_front(&[9, 8]);
        assert_eq!(&*b, &[9, 8, 2, 3]);
        b.extend_from_slice(&[4]);
        b.splice(1, 3, &[7]);
        assert_eq!(&*b, &[9, 7, 3, 4]);
        b.truncate(2);
        b.resize(3, 5);
        assert_eq!(&*b, &[9, 7, 5]);
        assert_eq!(b.clone().into_vec(), vec![9, 7, 5]);
    }

    #[test]
    fn a_drained_queue_compacts_and_empties() {
        let mut b = BufBytes::from((0..=255u8).collect::<Vec<_>>());
        for i in 0..200usize {
            assert_eq!(b[0] as usize, i);
            b.drop_front(1);
        }
        assert!(b.head < b.vec.len(), "head stays inside the storage");
        assert_eq!(b.len(), 56);
        b.drop_front(1000);
        assert!(b.is_empty());
        assert_eq!(b.head, 0);
    }

    #[test]
    fn repeated_unshift_reuses_front_slack() {
        let mut b = BufBytes::new();
        for i in 0..1000u32 {
            b.insert_front(&[(i % 251) as u8]);
        }
        assert_eq!(b.len(), 1000);
        assert_eq!(b[0], (999 % 251) as u8);
        assert_eq!(b[999], 0);
        assert_eq!(b.as_ptr(), b.vec[b.head..].as_ptr());
    }
}

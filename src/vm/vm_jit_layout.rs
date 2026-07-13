//! Runtime-discovered memory layout facts for the Tier B inline emitter
//! (ADR-0004 §2.3 Tier B).
//!
//! Tier B native code manipulates the VM value stack (`Interpreter::stack`,
//! a `Vec<Value>`) directly: it loads the data pointer / length words, reads
//! and writes 8-byte NaN-box value words in place, and adjusts the length.
//! `Interpreter` field offsets come from `offset_of!`; `Vec`'s internal field
//! order is unspecified by Rust, so it is probed once per process from a live
//! vector and self-verified. If the probe cannot pin all three words (an
//! exotic future `Vec` layout), Tier B inlining is disabled and every opcode
//! falls back to the Tier A helper-call form — never wrong, only slower.

use super::*;

pub(super) struct JitLayout {
    /// Byte offset of `Interpreter::stack` (a `Vec<Value>`).
    pub(super) stack: i32,
    /// Byte offsets of the data pointer / length / capacity words inside
    /// `Vec<Value>` (relative to the start of the `Vec`).
    pub(super) vec_ptr: i32,
    pub(super) vec_len: i32,
    pub(super) vec_cap: i32,
}

/// Identify where `Vec<Value>`'s (ptr, len, cap) words live by building a
/// vector whose three words are pairwise distinct and matching each by value.
fn probe_vec_layout() -> Option<(i32, i32, i32)> {
    const WORD: usize = std::mem::size_of::<usize>();
    const {
        assert!(std::mem::size_of::<Vec<Value>>() == 3 * WORD);
    }
    let mut v: Vec<Value> = Vec::with_capacity(7);
    v.push(Value::int(1));
    v.push(Value::int(2));
    v.push(Value::int(3));
    let data_ptr = v.as_ptr() as usize;
    // SAFETY: plain read of the Vec's own 3-word header (no aliasing, no
    // mutation); interpreting the words is what the value-matching below is
    // for, and any mismatch returns None instead of guessing.
    let words: [usize; 3] = unsafe { std::mem::transmute_copy(&v) };
    let (mut ptr_off, mut len_off, mut cap_off) = (None, None, None);
    for (i, w) in words.iter().enumerate() {
        let off = (i * WORD) as i32;
        if *w == data_ptr {
            ptr_off = Some(off);
        } else if *w == 3 {
            len_off = Some(off);
        } else if *w == 7 {
            cap_off = Some(off);
        }
    }
    Some((ptr_off?, len_off?, cap_off?))
}

/// The process-wide layout table, or `None` when Tier B must stay disabled.
pub(super) fn layout() -> Option<&'static JitLayout> {
    static LAYOUT: std::sync::OnceLock<Option<JitLayout>> = std::sync::OnceLock::new();
    LAYOUT
        .get_or_init(|| {
            let (vec_ptr, vec_len, vec_cap) = probe_vec_layout()?;
            Some(JitLayout {
                stack: std::mem::offset_of!(Interpreter, stack) as i32,
                vec_ptr,
                vec_len,
                vec_cap,
            })
        })
        .as_ref()
}

#[cfg(test)]
mod tests {
    /// The probe must succeed on the running std: Tier B silently degrading
    /// to helper calls on a mainstream toolchain would be a perf regression
    /// nobody notices, so pin it.
    #[test]
    fn vec_layout_probe_resolves() {
        let l = super::layout().expect("Vec<Value> layout probe failed");
        let offs = [l.vec_ptr, l.vec_len, l.vec_cap];
        for o in offs {
            assert!(o >= 0 && o <= 16 && o % 8 == 0, "weird offset {o}");
        }
        assert_ne!(l.vec_ptr, l.vec_len);
        assert_ne!(l.vec_len, l.vec_cap);
    }
}

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
    /// Byte offset of the slot `Vec<Value>` inside `Interpreter::locals` — the
    /// Tier B GetLocal fast path reads slot words in place (ADR-0004 J4d).
    /// `locals` is a [`crate::runtime::Locals`] (ADR-0077): one shared stack
    /// plus the executing frame's base, so this is the *stack*, and slot `i` of
    /// the current frame is at `locals_base + i`.
    pub(super) locals: i32,
    /// Byte offset of the executing frame's base word (a `usize`) inside
    /// `Interpreter::locals`. Tier B adds it to every slot index and
    /// bounds-checks the sum against the stack's length; it changes on every
    /// call, so it is loaded per access exactly like the `Vec` header words.
    pub(super) locals_base: i32,
    /// Byte offsets of the data pointer / length / capacity words inside
    /// `Vec<Value>` (relative to the start of the `Vec`).
    pub(super) vec_ptr: i32,
    pub(super) vec_len: i32,
    pub(super) vec_cap: i32,
    /// Byte offset of the DISCRIMINANT word of
    /// `Interpreter::test_pending_callsite_line` (an `Option<i64>`), so an
    /// inlined opcode can set the field to `None` with a single store.
    ///
    /// The `NqpOp` fast path needs it: `exec_nqp_op` clears that field on every
    /// nqp op, and an inline path that skipped the clear would leave a stale
    /// assertion line behind for whatever reads it next. `Option<i64>`'s layout
    /// is not guaranteed by Rust — the tag may sit at either word — so it is
    /// probed and the probe *verifies the write*, not just the position.
    ///
    /// `None` when the probe could not pin it. Only the paths that need it are
    /// disabled then; the rest of Tier B is unaffected.
    pub(super) pending_line_tag: Option<i32>,
}

/// Find the discriminant word of an `Option<i64>` and confirm that zeroing it
/// really does produce `None`.
///
/// Same discipline as [`probe_vec_layout`]: match by value rather than assume
/// a field order, and answer `None` rather than guess. Unlike that one this
/// also performs the write it is licensing, because "which word is the tag" and
/// "does storing 0 there mean `None`" are two different facts and only the
/// second is what the emitted code relies on.
fn probe_option_i64_tag() -> Option<i32> {
    const WORD: usize = std::mem::size_of::<usize>();
    if std::mem::size_of::<Option<i64>>() != 2 * WORD {
        return None;
    }
    // A payload no tag could plausibly equal, so the two words are telling apart.
    const PAYLOAD: i64 = 0x5AA5_1234_5678;
    let some: Option<i64> = Some(PAYLOAD);
    // SAFETY: a plain read of the enum's own two words; the value-matching
    // below is what interprets them, and any mismatch returns None.
    let words: [u64; 2] = unsafe { std::mem::transmute_copy(&some) };
    let tag_off = match (words[0] == PAYLOAD as u64, words[1] == PAYLOAD as u64) {
        // The payload is in word 1, so word 0 is the tag, and vice versa.
        (false, true) => 0,
        (true, false) => WORD as i32,
        // Both or neither matched: cannot tell them apart.
        _ => return None,
    };
    if words[tag_off as usize / WORD] == 0 {
        // A `Some` whose tag word already reads zero would make the store a
        // no-op rather than a clear.
        return None;
    }
    // Perform the exact write the emitted code will, and check it lands.
    let mut probe: Option<i64> = Some(PAYLOAD);
    // SAFETY: `probe` is a live, uniquely-owned `Option<i64>`; the offset is
    // within its size (checked above) and `u64` has no stricter alignment than
    // the enum's own. Writing the discriminant is exactly what the emitted
    // store does, which is the point of doing it here.
    unsafe {
        let base = (&raw mut probe).cast::<u8>();
        base.add(tag_off as usize).cast::<u64>().write(0);
    }
    if probe.is_some() {
        return None;
    }
    Some(tag_off)
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
                locals: (std::mem::offset_of!(Interpreter, locals)
                    + crate::runtime::Locals::SLOTS_BYTE_OFFSET) as i32,
                locals_base: (std::mem::offset_of!(Interpreter, locals)
                    + crate::runtime::Locals::BASE_BYTE_OFFSET) as i32,
                vec_ptr,
                vec_len,
                vec_cap,
                pending_line_tag: probe_option_i64_tag().map(|off| {
                    std::mem::offset_of!(Interpreter, test_pending_callsite_line) as i32 + off
                }),
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
            assert!((0..=16).contains(&o) && o % 8 == 0, "weird offset {o}");
        }
        assert_ne!(l.vec_ptr, l.vec_len);
        assert_ne!(l.vec_len, l.vec_cap);
    }

    /// The `NqpOp` fast path clears the pending assertion line with one store,
    /// which is only licensed while this probe resolves. Silently losing it
    /// would send every nqp op back through the helper, so pin it.
    #[test]
    fn option_i64_tag_probe_resolves_and_the_write_clears() {
        let off = super::probe_option_i64_tag().expect("Option<i64> tag probe failed");
        assert!(off == 0 || off == 8, "weird tag offset {off}");
        let l = super::layout().expect("layout probe failed");
        assert_eq!(
            l.pending_line_tag,
            Some(
                std::mem::offset_of!(crate::runtime::Interpreter, test_pending_callsite_line)
                    as i32
                    + off
            )
        );
    }
}

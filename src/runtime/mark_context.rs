//! The "mark context" one-shot flag family, packed into one word.
//!
//! `MarkBindContext` and its siblings are compiler-emitted opcodes that set a
//! single interpreter-wide flag immediately before a `:=`/vardecl target's own
//! store op, to be consumed by that very next store. Because a real call can
//! sit between the mark and its consumer, every call-dispatch path has to
//! isolate the whole family across the call boundary
//! ([`crate::vm::vm_call_state_guard::MarkContextGuard`]).
//!
//! Each flag used to be its own `Box<Cell<bool>>` field on `Interpreter`. That
//! made the guard ten raw pointers plus ten saved values — ~120 bytes of the
//! hot positional-light call's stack frame, ~20 loads to construct and ~10
//! stores to drop, per call, for a family that is all-false at essentially
//! every call boundary (#7738).
//!
//! Packing the nine booleans into one `u16` bitfield, in the *same* heap
//! allocation as the one non-`Copy` member (`array_share_source`), turns the
//! guard into one pointer, one `u16` and one `Option<String>`: save is a single
//! load, clear a single store, restore a single store.
//!
//! The `Box` indirection is load-bearing and must stay — see
//! [`crate::vm::vm_call_state_guard`]'s module doc ("v3"): the guard's `Drop`
//! reaches this state through a raw pointer, and only a heap allocation
//! *separate* from `Interpreter`'s own survives the Stacked-Borrows retag that
//! every later `&mut self` call performs over `Interpreter`'s whole byte range.
//! Collapsing ten such allocations into one keeps that property (it is still
//! one `Box`, disjoint from `Interpreter`) while paying for one pointer chase
//! instead of ten.

use std::cell::Cell;

/// Bit positions in [`MarkContextState::flags`].
///
/// The values are private to this module: every reader goes through the
/// [`crate::runtime::Interpreter`] accessor for its flag, which hands out a
/// [`MarkFlag`] bound to the right bit.
pub(crate) mod bit {
    pub(crate) const BIND: u16 = 1 << 0;
    pub(crate) const SCALAR_BIND: u16 = 1 << 1;
    pub(crate) const PARAM_RAW_BIND: u16 = 1 << 2;
    pub(crate) const BOUND_DECONT_ACTIVE: u16 = 1 << 3;
    pub(crate) const REBIND: u16 = 1 << 4;
    pub(crate) const CONSTANT: u16 = 1 << 5;
    pub(crate) const ARRAY_SHARE: u16 = 1 << 6;
    pub(crate) const EXPLICIT_INITIALIZER: u16 = 1 << 7;
    pub(crate) const VARDECL: u16 = 1 << 8;
}

/// The whole mark-context flag family in one allocation.
#[derive(Default)]
pub(crate) struct MarkContextState {
    /// The nine boolean flags, one per [`bit`] constant.
    pub(crate) flags: Cell<u16>,
    /// Slice 2a/2b (`docs/scalar-array-sharing.md`): the source variable name
    /// whose container the upcoming `SetLocal`/`AssignExpr` should share (set
    /// by `MarkArrayShareSource`). `@z`/`%h` for a whole-container RHS
    /// (`$n = @z`), or a scalar name for a chained share (`$r = $q`); the
    /// runtime only shares when that source holds a container/`ContainerRef`
    /// (so a plain `$x = $y` stays a copy).
    ///
    /// Not `Copy`, so it stays a `Cell<Option<String>>` of its own rather than
    /// joining the bitfield — but it lives in this same allocation, so the
    /// guard still needs only one raw pointer.
    pub(crate) share_source: Cell<Option<String>>,
}

impl MarkContextState {
    /// Save the whole family and clear it, as a call boundary does.
    #[inline]
    pub(crate) fn take_all(&self) -> (u16, Option<String>) {
        let flags = self.flags.get();
        self.flags.set(0);
        // `Cell::take` on a `None` is a 24-byte move either way; there is no
        // cheaper peek on a `Cell`, and the bit that would gate it
        // (`ARRAY_SHARE`) is set independently of the source (`SetLocal`
        // consumes the flag but leaves the name behind).
        (flags, self.share_source.take())
    }

    /// Every member a store consumes -- the whole family except
    /// `bound_decont_active`. That one is not a one-shot mark at all but a
    /// sticky cheap-gate ("some `__mutsu_bound_decont::` marker exists", set
    /// by `update_bound_decont_marker` and read by `ItemizeVar`), so a store
    /// must leave it standing. Left alone here for exactly that reason, as
    /// the eight separate `set(false)` statements this replaces also did.
    const CONSUMED_BY_STORE: u16 = bit::BIND
        | bit::SCALAR_BIND
        | bit::PARAM_RAW_BIND
        | bit::REBIND
        | bit::CONSTANT
        | bit::ARRAY_SHARE
        | bit::EXPLICIT_INITIALIZER
        | bit::VARDECL;

    /// Whether *no* store-flavour mark is pending: the next store is a plain
    /// `=` into an already-declared variable, with no bind, rebind, `constant`,
    /// declaration, initializer or array-share flavour to apply.
    ///
    /// One load and one test, and — because there is then nothing to clear —
    /// it leaves the word alone, which is what lets the plain-scalar store fast
    /// path decide its whole flavour question before it touches the stack.
    #[inline]
    pub(crate) fn store_flags_clear(&self) -> bool {
        self.flags.get() & Self::CONSUMED_BY_STORE == 0
    }

    /// Snapshot the flag word and clear everything a store consumes, in one
    /// load and one store. See [`MarkFlags`].
    #[inline]
    pub(crate) fn consume_for_store(&self) -> MarkFlags {
        let w = self.flags.get();
        self.flags.set(w & !Self::CONSUMED_BY_STORE);
        MarkFlags(w)
    }

    /// Restore what [`MarkContextState::take_all`] saved.
    #[inline]
    pub(crate) fn restore_all(&self, flags: u16, share_source: Option<String>) {
        self.flags.set(flags);
        self.share_source.set(share_source);
    }
}

/// The mark flags as one `SetLocal`/`SetGlobal` consumer sees them: a snapshot
/// of the whole word taken at the moment the store consumed (and cleared) it.
///
/// A store reads seven or eight members of the family and then clears every
/// one — each `Mark*` op's flag is defined to be consumed by the very next
/// store. With the family packed, that is one load and one store instead of
/// eight of each, which is why this snapshot type exists rather than eight
/// separate [`MarkFlag`] reads.
#[derive(Clone, Copy, Debug)]
pub(crate) struct MarkFlags(u16);

impl MarkFlags {
    #[inline]
    pub(crate) fn bind(self) -> bool {
        self.0 & bit::BIND != 0
    }
    #[inline]
    pub(crate) fn scalar_bind(self) -> bool {
        self.0 & bit::SCALAR_BIND != 0
    }
    #[inline]
    pub(crate) fn param_raw_bind(self) -> bool {
        self.0 & bit::PARAM_RAW_BIND != 0
    }
    #[inline]
    pub(crate) fn rebind(self) -> bool {
        self.0 & bit::REBIND != 0
    }
    #[inline]
    pub(crate) fn constant(self) -> bool {
        self.0 & bit::CONSTANT != 0
    }
    #[inline]
    pub(crate) fn array_share(self) -> bool {
        self.0 & bit::ARRAY_SHARE != 0
    }
    #[inline]
    pub(crate) fn explicit_initializer(self) -> bool {
        self.0 & bit::EXPLICIT_INITIALIZER != 0
    }
    #[inline]
    pub(crate) fn vardecl(self) -> bool {
        self.0 & bit::VARDECL != 0
    }
}

/// One flag of [`MarkContextState::flags`], addressed as if it were still its
/// own `Cell<bool>`.
///
/// Zero-cost: a shared borrow plus a compile-time-constant mask, so
/// `interp.bind_context().get()` compiles to the same load-and-test the
/// separate-`Cell` field did, minus one pointer chase.
#[derive(Clone, Copy)]
pub(crate) struct MarkFlag<'a> {
    word: &'a Cell<u16>,
    mask: u16,
}

impl<'a> MarkFlag<'a> {
    #[inline]
    pub(crate) fn new(word: &'a Cell<u16>, mask: u16) -> Self {
        MarkFlag { word, mask }
    }

    #[inline]
    pub(crate) fn get(self) -> bool {
        self.word.get() & self.mask != 0
    }

    #[inline]
    pub(crate) fn set(self, value: bool) {
        let w = self.word.get();
        self.word
            .set(if value { w | self.mask } else { w & !self.mask });
    }
}

/// The per-flag accessors. Each hands out a `MarkFlag` bound to its bit, so
/// a call site reads exactly as it did when every flag was its own
/// `Box<Cell<bool>>` field: `self.vardecl_context().get()`,
/// `self.bind_context().set(true)`.
impl crate::runtime::Interpreter {
    /// Set by `MarkBindContext` just before a `:=` target's own store op.
    #[inline]
    pub(crate) fn bind_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::BIND)
    }

    #[inline]
    pub(crate) fn scalar_bind_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::SCALAR_BIND)
    }

    /// Set by `MarkParamRawBindContext` just before the SetLocal/SetGlobal of
    /// an assignment whose target is a sigilless binding (`-> \v` loop-param
    /// bind statements, writes through a sigilless alias). Its ONLY effect is
    /// to skip scalar-store itemization — a sigilless name is a non-container
    /// alias, so the stored value must stay bare. No other bind semantics.
    #[inline]
    pub(crate) fn param_raw_bind_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::PARAM_RAW_BIND)
    }

    #[inline]
    pub(crate) fn bound_decont_active(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::BOUND_DECONT_ACTIVE)
    }

    #[inline]
    pub(crate) fn rebind_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::REBIND)
    }

    #[inline]
    pub(crate) fn constant_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::CONSTANT)
    }

    /// Slice 2a (`docs/scalar-array-sharing.md`): set by `MarkArrayShareContext`
    /// just before a `SetLocal` for `$scalar = @arr` / `$scalar = %hash`. Tells
    /// the assignment to promote the source container to a shared `ContainerRef`
    /// cell (raku reference semantics) rather than snapshotting it.
    #[inline]
    pub(crate) fn array_share_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::ARRAY_SHARE)
    }

    #[inline]
    pub(crate) fn explicit_initializer_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::EXPLICIT_INITIALIZER)
    }

    #[inline]
    pub(crate) fn vardecl_context(&self) -> MarkFlag<'_> {
        MarkFlag::new(&self.mark_ctx.flags, bit::VARDECL)
    }

    /// The one non-`Copy` member of the family — see
    /// `MarkContextState::share_source`.
    #[inline]
    pub(crate) fn array_share_source(&self) -> &Cell<Option<String>> {
        &self.mark_ctx.share_source
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flags_are_independent() {
        let st = MarkContextState::default();
        let bind = MarkFlag::new(&st.flags, bit::BIND);
        let vardecl = MarkFlag::new(&st.flags, bit::VARDECL);
        assert!(!bind.get());
        assert!(!vardecl.get());

        bind.set(true);
        assert!(bind.get());
        assert!(!vardecl.get(), "setting one flag must not set another");

        vardecl.set(true);
        bind.set(false);
        assert!(!bind.get());
        assert!(vardecl.get(), "clearing one flag must not clear another");
    }

    /// Every bit constant is distinct — a duplicated mask would silently alias
    /// two unrelated flags.
    #[test]
    fn bits_are_distinct() {
        let all = [
            bit::BIND,
            bit::SCALAR_BIND,
            bit::PARAM_RAW_BIND,
            bit::BOUND_DECONT_ACTIVE,
            bit::REBIND,
            bit::CONSTANT,
            bit::ARRAY_SHARE,
            bit::EXPLICIT_INITIALIZER,
            bit::VARDECL,
        ];
        let mut seen: u16 = 0;
        for b in all {
            assert_eq!(b.count_ones(), 1, "each flag owns exactly one bit");
            assert_eq!(seen & b, 0, "bit {b:#x} is used twice");
            seen |= b;
        }
    }

    #[test]
    fn take_all_clears_and_restore_puts_back() {
        let st = MarkContextState::default();
        MarkFlag::new(&st.flags, bit::REBIND).set(true);
        MarkFlag::new(&st.flags, bit::ARRAY_SHARE).set(true);
        st.share_source.set(Some("@z".to_string()));

        let (flags, src) = st.take_all();
        assert_eq!(st.flags.get(), 0, "the callee starts with a clear family");
        assert_eq!(st.share_source.take(), None);

        // A callee's own leftovers are discarded by the restore.
        MarkFlag::new(&st.flags, bit::BIND).set(true);
        st.share_source.set(Some("%callee".to_string()));
        st.restore_all(flags, src);

        assert!(MarkFlag::new(&st.flags, bit::REBIND).get());
        assert!(MarkFlag::new(&st.flags, bit::ARRAY_SHARE).get());
        assert!(!MarkFlag::new(&st.flags, bit::BIND).get());
        assert_eq!(st.share_source.take(), Some("@z".to_string()));
    }
}

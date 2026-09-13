//! ADR-0097 slice 1: a binding's own metadata, addressed by its compile-time
//! local slot, instead of thirteen name-derived vectors on `CompiledCode`.
//!
//! Before this, `CompiledCode` carried five separate `Vec<Symbol>` — one per
//! `__mutsu_*` env-key namespace a scalar store or declaration probes on
//! every access — plus two `Vec<bool>` bitmaps (`plain_locals`,
//! `simple_scalar_locals`) answering questions later campaigns found they
//! needed about the same slot. Each was added by a different perf campaign,
//! each indexed by the same local slot number, and none of them referenced
//! each other. This module folds all seven into one `Vec<BindingDesc>`, one
//! entry per local slot, so the next property a campaign needs is a field on
//! an existing struct rather than a fourteenth parallel vector.
//!
//! This slice is a pure consolidation: no `__mutsu_*` namespace is retired
//! and no probe changes shape. [`crate::opcode::CompiledCode::alias_sym`] and
//! its four siblings, and the `is_plain_local`/`is_simple_scalar_local` predicates,
//! keep their exact prior fallback behaviour for a hand-built chunk whose
//! `locals` outgrew its descriptors (see each accessor's own doc comment).
//! Later ADR-0097 slices grow [`BindingDesc`] with the declaration-settled
//! and binding-shape properties currently still living under their own
//! `__mutsu_*` `Env` keys, retiring one `MetaNs` namespace per field added.

use crate::symbol::Symbol;

/// Bitmap of the two per-slot facts that are settled by a scan of the slot's
/// *name* alone, and therefore fixed at `Compiler::alloc_fresh_local` time for
/// the life of the slot.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct BindingFlags(u8);

impl BindingFlags {
    /// See [`BindingFlags::plain_local`].
    const PLAIN_LOCAL: u8 = 0b01;
    /// See [`BindingFlags::simple_scalar_local`].
    const SIMPLE_SCALAR_LOCAL: u8 = 0b10;

    #[inline]
    pub(crate) fn new(plain_local: bool, simple_scalar_local: bool) -> Self {
        let mut bits = 0;
        if plain_local {
            bits |= Self::PLAIN_LOCAL;
        }
        if simple_scalar_local {
            bits |= Self::SIMPLE_SCALAR_LOCAL;
        }
        BindingFlags(bits)
    }

    /// True if the slot's name is a *plain lexical* — see
    /// `Compiler::is_plain_lexical_name` for the exact predicate. Was
    /// `CompiledCode::plain_locals[idx]`.
    #[inline]
    pub(crate) fn plain_local(self) -> bool {
        self.0 & Self::PLAIN_LOCAL != 0
    }

    /// True if a store into the slot may take the plain-scalar fast path in
    /// `exec_set_local_op` — see `Compiler::is_simple_scalar_store_name`. A
    /// strict subset of [`Self::plain_local`]. Was
    /// `CompiledCode::simple_scalar_locals[idx]`.
    #[inline]
    pub(crate) fn simple_scalar_local(self) -> bool {
        self.0 & Self::SIMPLE_SCALAR_LOCAL != 0
    }
}

/// One local slot's compile-time-settled metadata. `CompiledCode::binding_descs`
/// holds one per entry of `CompiledCode::locals`, same index.
///
/// The five `Option<Symbol>` fields are `None` until
/// [`crate::opcode::CompiledCode::compute_locals_sym`] runs (deliberately —
/// see that method), at which point every slot gets `Some`. Each accessor on
/// `CompiledCode` (`alias_sym`, `readonly_sym`, `deleted_index_sym`,
/// `bound_slice_sym`, `scalar_no_container_sym`) falls back to deriving the
/// key from the slot's name on a `None`, exactly as it did when the field was
/// a separate, possibly-shorter `Vec<Symbol>`.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct BindingDesc {
    /// The interned `__mutsu_sigilless_alias::<name>` env key.
    pub(crate) alias_sym: Option<Symbol>,
    /// The interned `__mutsu_sigilless_readonly::<name>` env key.
    pub(crate) readonly_sym: Option<Symbol>,
    /// The interned `__mutsu_deleted_index::<name>` env key.
    pub(crate) deleted_index_sym: Option<Symbol>,
    /// The interned `__mutsu_bound_array_slice::<name>` env key.
    pub(crate) bound_slice_sym: Option<Symbol>,
    /// The interned `__mutsu_scalar_bind_no_container::<name>` env key.
    pub(crate) scalar_no_container_sym: Option<Symbol>,
    pub(crate) flags: BindingFlags,
}

impl BindingDesc {
    #[inline]
    pub(crate) fn new(flags: BindingFlags) -> Self {
        BindingDesc {
            flags,
            ..Default::default()
        }
    }
}

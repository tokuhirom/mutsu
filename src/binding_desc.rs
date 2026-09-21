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

/// What a slot's own declarations settle about its type constraint, as far as
/// the scalar-store fast path is concerned.
///
/// `my int $i` fixes `$i`'s constraint at its declaration, and the compiler is
/// the one that reads it. Before this, every store to `$i` re-derived it at run
/// time — hash `__mutsu_type::i`, walk the env chain, decode the `Str` it finds
/// — at ~130 instructions, **22% of the store and 9% of an `nqp::add_i` loop**,
/// to re-learn a fact settled once at compile time
/// ([#8877](https://github.com/tokuhirom/mutsu/issues/8877)).
///
/// The variants are deliberately coarse, because the store asks exactly one
/// question — `native_typed_store_is_identity`, "would the typed branch leave
/// this value alone?" — and its answer depends only on which native family the
/// constraint belongs to, never on the spelling.
///
/// **The env entry is not retired.** It stays the source of truth for the ~120
/// other readers, and the fallback here: a slot whose constraint this chunk did
/// not declare, or declared in a way the bake cannot describe, reads
/// [`Self::Unrecorded`] / [`Self::Conflicting`] and probes exactly as before.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) enum DeclaredConstraint {
    /// No declaration in this chunk registers a constraint for this slot — ask
    /// the env. Every slot starts here, and a slot the chunk merely *uses* (a
    /// free variable, a parameter, a compiler temporary) stays here: the
    /// constraint, if there is one, belongs to whatever declared it.
    #[default]
    Unrecorded,
    /// `int` / `int64`.
    NativeInt,
    /// `str`.
    NativeStr,
    /// `num` / `num64`.
    NativeNum,
    /// A constraint outside the five above — `Int`, `int8`, a class, a subset
    /// name. The typed branch always has real work to do for it (the narrow
    /// widths wrap, everything else type-checks), so the fast path can decline
    /// without asking the env, which is what the probe concluded anyway.
    NonNative,
    /// Two *different* constraints were recorded for the same slot, or a trait
    /// that can rewrite the constraint at run time was applied to it. The
    /// default build gives a nested `my $x` the **outer** slot
    /// (`Compiler::declare_local`), so `{ my int $i } { my Int $i }` really is
    /// one slot with two constraints; the env, which is scoped, can tell them
    /// apart and the bake cannot. Falls back to the probe.
    Conflicting,
}

impl DeclaredConstraint {
    /// Classify a declaration's constraint text.
    ///
    /// The comparison is against the same string the env entry will hold:
    /// `Interpreter::parse_container_constraint` stores a scalar's constraint
    /// as the trimmed raw text, so a bake that trims matches it exactly.
    pub(crate) fn classify(tc: &str) -> Self {
        match tc.trim() {
            "int" | "int64" => DeclaredConstraint::NativeInt,
            "str" => DeclaredConstraint::NativeStr,
            "num" | "num64" => DeclaredConstraint::NativeNum,
            _ => DeclaredConstraint::NonNative,
        }
    }

    /// Fold another declaration's classification into this slot's record.
    /// Agreement keeps the record, disagreement poisons it, and
    /// [`Self::Conflicting`] is absorbing — a trait poisons a slot whose
    /// deferred `is default(...)` type registration is emitted *after* it.
    pub(crate) fn merge(&mut self, other: Self) {
        *self = match *self {
            DeclaredConstraint::Unrecorded => other,
            DeclaredConstraint::Conflicting => DeclaredConstraint::Conflicting,
            cur if cur == other => cur,
            _ => DeclaredConstraint::Conflicting,
        };
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
    /// What this slot's own declarations settle about its type constraint —
    /// see [`DeclaredConstraint`]. `Unrecorded` (the default) means "ask the
    /// env", which is what every slot did before.
    pub(crate) declared_constraint: DeclaredConstraint,
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

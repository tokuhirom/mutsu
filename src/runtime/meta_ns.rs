//! The one constructor for a `__mutsu_*` metadata env key (issue #8087).
//!
//! # Why this type exists
//!
//! A large amount of per-binding metadata — is this name a sigilless alias, is
//! it `:=`-bound, does it carry a type or key-type constraint, is it a `state`
//! variable, is its array shaped — is not stored *on* the binding. It is stored
//! as a sibling entry in the same string-keyed [`Env`](crate::env::Env), under a
//! key derived from the variable's own name. So the only way to get from a
//! binding to its own metadata is to build that derived name and probe the env
//! with it, and the obvious way to build it is `format!`.
//!
//! That has been the profiling finding in **five separate perf campaigns**:
//! the `bench-ctor` atomic-lane probe; `Digest::RIPEMD` ([#7571]), where
//! rebuilding unchanging key strings per closure call was ~3.2% of the run;
//! the typed-lexical probes ([#7766]), probed up to seven times per store;
//! `bench-ctor` name re-derivation, at *146 name interns per constructed
//! object*; and the element store ([#8069]), at **22 interns and 8 heap
//! allocations per `@a[$i] = $v`**. Each was fixed by memoizing the one key the
//! profile happened to walk through, which is why it kept coming back: the
//! memoized helpers existed, and 96 call sites still built their key by hand.
//! There are none left. The gate that tracked them ran 276 -> 174 -> 0 across
//! the three stages of [#8087], and with the last of them converted
//! `scripts/check-magic-keys.sh` stopped being a ratchet over a baseline and
//! became a plain ban: this module is the only place a `__mutsu_*` metadata key
//! is spelled.
//!
//! # What it does
//!
//! [`MetaNs`] enumerates the namespaces and [`MetaNs::key`] is the only way to
//! obtain a key `Symbol` for one. The `(namespace, name) -> key` mapping never
//! changes — symbols are append-only — so it is memoized per thread and the
//! `format!` runs once per pair for the life of the process.
//!
//! This deliberately replaces what used to be seven near-identical
//! `*_key_for_sym` helpers plus `shared_store::atomic_lane_key`'s own pair of
//! tables, each with its own copy of the same memo boilerplate. Two tables now
//! — one for the single-name namespaces, one for the two-part ones — one place
//! to add a namespace, and adding the next is correct by construction rather
//! than by remembering.
//!
//! A handful of namespaces are keyed by an *identity* rather than by a name —
//! a `Seq`'s id, a flip-flop's dynamic scope, a `state` variable's routine
//! clone id, an inline-package declaration site's fingerprint. Those go through
//! [`MetaNs::key_for_id`] and [`MetaNs::owned_key_from_parts`], which build the
//! string every time on
//! purpose: the set of such keys grows with what the program *does* rather than
//! with what it says, so memoizing one would be a map that only ever grows.
//! They are here for the other half of the job — one place that spells the
//! prefix — not for the memo.
//!
//! # What this is not about: gensyms
//!
//! `__mutsu_tmp_if_cond_7`, `__mutsu_sm_val_3`, `__mutsu_chain_cmp_0` and the
//! forty-odd names like them are NOT keys and do not belong here. They are
//! gensyms: one fresh unique local name per compile site, handed straight to
//! `alloc_local`, never rebuilt and never probed. The shape is what tells them
//! apart, and the codebase keeps the distinction without exception — a key puts
//! `::` (env) or `__` (mixin registry) before the name it derives from, a
//! gensym puts a single `_` before a counter. `scripts/check-magic-keys.sh`
//! draws exactly that line.
//!
//! # What it is NOT
//!
//! It is **not the fix**. The fix is for these keys to stop existing: every one
//! of them is a property of a single binding and belongs on that binding's
//! resolved descriptor ([#8069] §4.1) or off the per-frame env entirely
//! ([#7817] / ADR-0084). Memoizing the construction makes the current design
//! cheap, which is a smaller good than deleting it — so treat this as the
//! staging ground for that move, not as the destination. With every access
//! funnelled through one enum, retiring a namespace becomes a change at one
//! site instead of at twenty-eight — which is now literally true for every
//! namespace below: none of them has a hand-built site left anywhere in `src/`,
//! so stage 4 can delete them one at a time as the metadata moves onto the
//! binding.
//!
//! `scripts/check-magic-keys.sh` is what keeps that ground from being lost.
//!
//! [#7571]: https://github.com/tokuhirom/mutsu/issues/7571
//! [#7766]: https://github.com/tokuhirom/mutsu/issues/7766
//! [#7817]: https://github.com/tokuhirom/mutsu/issues/7817
//! [#8069]: https://github.com/tokuhirom/mutsu/issues/8069
//! [#8087]: https://github.com/tokuhirom/mutsu/issues/8087

use crate::symbol::Symbol;

/// A `__mutsu_*` per-binding metadata namespace.
///
/// Every namespace is listed, and each one is migrated *completely*: no
/// `format!("__mutsu_<ns>::…")` for any of them is left in `src/`, which
/// `scripts/check-magic-keys.sh` enforces. Add a variant when a new namespace
/// is needed — and pin its exact spelling in
/// `every_namespace_spells_its_key_exactly_as_the_format_sites_did`, because a
/// prefix typo is invisible: the writer stores under one key, the reader probes
/// another, and the metadata is simply never found.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum MetaNs {
    /// `__mutsu_sigilless_alias::<name>` — the `@`/`%`/`$` name a sigilless
    /// binding (`my \h = %a`) actually aliases.
    SigillessAlias,
    /// `__mutsu_sigilless_readonly::<name>` — whether a sigilless binding
    /// refuses assignment.
    SigillessReadonly,
    /// `__mutsu_type::<name>` — a lexical's declared type constraint.
    Type,
    /// `__mutsu_hash_key_type::<name>` — an object hash's key type
    /// (`my %h{Int}`).
    HashKeyType,
    /// `__mutsu_state_key::<name>` — the shared cell a `state` variable
    /// resolves to.
    State,
    /// `__mutsu_bound::<name>` — the name was installed by `:=`, which makes a
    /// readonly container mutable in place (as against a `constant`).
    Bound,
    /// `__mutsu_bound_index::<name>` — an element of this container is itself
    /// `:=`-bound, so an element store must not replace the container.
    BoundIndex,
    /// `__mutsu_shaped_array_dims::<name>` — the declared dimensions of a
    /// shaped array (`my @a[4;4]`).
    ShapedArrayDims,
    /// `__mutsu_atomic_arr::<name>` — the cross-thread lane a shared `@a`
    /// resolves to in the shared store.
    AtomicArr,
    /// `__mutsu_atomic_hash::<name>` — the lane for a shared `%h`.
    AtomicHash,
    /// `__mutsu_callable_id::<package>::<name>` — a routine's registration
    /// clone id. Built with [`MetaNs::key_pair`].
    CallableId,
    /// `__mutsu_array_share::<name>` — a `$n = @z` scalar holding a shared
    /// container, so a whole reassignment replaces the slot.
    ArrayShare,
    /// `__mutsu_atomic_name::<name>` — the shared-store value key a scalar
    /// touched by an atomic op resolves to.
    AtomicName,
    /// `__mutsu_atomic_value::<id>` — the shared-store slot an atomic scalar's
    /// value lives in. Keyed by id, not by a variable name.
    AtomicValue,
    /// `__mutsu_bound_array_len::<name>` — the element count a `bind-array`
    /// target was bound at.
    BoundArrayLen,
    /// `__mutsu_bound_array_slice::<name>` — the name is a genuine bound array
    /// slice (`@slice := @a[1,2]`).
    BoundArraySlice,
    /// `__mutsu_bound_decont::<name>` — a `$x := @a` scalar that must
    /// decontainerize on read.
    BoundDecont,
    /// `__mutsu_compunit::<prefix>::<short-name>` — a loaded compunit, cached
    /// per repository prefix. Built with [`MetaNs::key_pair`].
    Compunit,
    /// `__mutsu_constant_var::<name>` — the name was declared `constant`, which
    /// makes it a compile-time value rather than merely readonly.
    ConstantVar,
    /// `__mutsu_deep_readonly::<name>` — the binding refuses method-based
    /// mutation too (`.value = …` on a readonly `Pair`).
    DeepReadonly,
    /// `__mutsu_deleted_index::<name>` — the set of indices `:delete`d out of
    /// this container, which read as missing even when the slot holds a type
    /// object.
    DeletedIndex,
    /// `__mutsu_elem_share::<name>` — the set of this container's indices that
    /// hold a shared cell an element store must write *through*.
    ElemShare,
    /// `__mutsu_eval_role::<qualified-name>` — a role declared inside an `EVAL`
    /// with `my` scope.
    EvalRole,
    /// `__mutsu_ff_state::<scope>::<site-id>` — one flip-flop operator's latch
    /// state, per dynamic scope. Built with [`MetaNs::key_pair`].
    FfState,
    /// `__mutsu_gather_self_ref::<name>` — the name is the aggregate a `gather`
    /// is being assigned to, so a self-mention inside must not reify it.
    GatherSelfRef,
    /// `__mutsu_inline_package_proto_preregistered::<package>::<name>` — an
    /// `our proto` the package prepass already installed. Built with
    /// [`MetaNs::key_pair`].
    InlinePackageProto,
    /// `__mutsu_inline_package_sub_preregistered::<package>::<name>::<site>` —
    /// the same marker for a `multi` candidate, which needs the declaration
    /// site's fingerprint as a third part. Built with
    /// [`MetaNs::owned_key_from_parts`], including the prefix-scan form that
    /// leaves the fingerprint empty.
    InlinePackageSub,
    /// `__mutsu_scalar_bind_no_container::<name>` — a `$x := 42` bound straight
    /// to a value, with no container to write through.
    ScalarBindNoContainer,
    /// `__mutsu_shared_state::<state-key>` — the cross-thread cell a `state`
    /// variable's value lives in once any thread has touched it. Its name half
    /// carries the routine's clone id, so it is built with
    /// [`MetaNs::owned_key_from_parts`] and deliberately not memoized.
    SharedState,
    /// `__mutsu_method_value::<name>` — the name was registered as a method
    /// rather than a plain sub.
    MethodValue,
    /// `__mutsu_outer::<name>` — the enclosing scope's value of `name`, snapped
    /// at closure capture so `$OUTER::name` can still see it.
    Outer,
    /// `__mutsu_predictive_seq_iter::<seq-id>` — a `Seq`'s live iterator, held
    /// across the sub/block return its `predictive_seq_iters` field cannot.
    /// Keyed by identity, not by a variable name.
    PredictiveSeqIter,
    /// `__mutsu_repo_fs::<canonical-prefix>` — a `CompUnit::Repository::FileSystem`
    /// object, cached per canonicalized prefix path.
    RepoFs,
    /// `__mutsu_ro_index::<name>` — the set of this container's indices that
    /// are readonly.
    RoIndex,
    /// `__mutsu_shared_dirty::<name>` — the shared-store entry for `name` has
    /// been written by another thread and the local mirror must re-read it.
    SharedDirty,
    /// `__mutsu_var_meta::<name>` — a lexical's `is dynamic` flag.
    VarMeta,
    /// `__mutsu_var_source_name::<name>` — the variable a `$_`-ish alias was
    /// derived from, so a write can be routed back to it.
    VarSourceName,
    /// `__mutsu_attr_alias::<attr>` — the public accessor name a private
    /// attribute is also reachable under.
    AttrAlias,

    // --- The mixin/role registry. These live in a `MixinOverrides` map keyed
    // by `String`, not in an `Env`, so their callers want
    // [`MetaNs::str_key_for_str`] rather than a `Symbol`. They join prefix to
    // name with no separator, which is why `prefix()` carries its own.
    /// `__mutsu_role__<role>` — the marker that a value does this role. The
    /// single most-probed key in the mixin map: every `does`, every method
    /// dispatch onto a mixin and every type smartmatch reads it.
    Role,
    /// `__mutsu_role_seq__<role>` — a monotonic application-order stamp, which
    /// decides later-wins precedence between two roles' same-named methods.
    RoleSeq,
    /// `__mutsu_role_id__<role>` — the `RoleDef` id this application resolved
    /// to, which differs per parameterisation.
    RoleId,
    /// `__mutsu_role_typeargs__<role>` — the type arguments a parameterised
    /// role was applied with.
    RoleTypeargs,
    /// `__mutsu_role_param__<param>` — one bound role type parameter.
    RoleParam,
    /// `__mutsu_role_group__<role>` — the candidate group a multi-candidate
    /// (multiply-declared) role belongs to.
    RoleGroup,
    /// `__mutsu_role_hides__<role>` — a `hides` relationship, recorded as a
    /// pseudo parent-role entry by the role declarator.
    RoleHides,
    /// `__mutsu_attr__<attr>` — a role attribute composed into the mixin.
    Attr,
    /// `__mutsu_attr_trait__<owner>!<attr>` — one attribute trait, keyed by the
    /// owning class and the attribute. Built with [`MetaNs::key_pair`], whose
    /// separator for this namespace is `!`, not `::`.
    AttrTrait,
    /// `__mutsu_type_capture_bound__<name>` — a `::T` type capture has been
    /// bound, as against `T` merely resolving to the literal name.
    TypeCaptureBound,
}

impl MetaNs {
    /// Every namespace, so a test can assert a property of all of them and a
    /// new variant cannot be added without appearing here.
    ///
    /// Test-only: the invariants it carries (no prefix is a prefix of another,
    /// every spelling is pinned) are checked once, not consulted at runtime.
    #[cfg(test)]
    pub(crate) const ALL: &'static [MetaNs] = &[
        MetaNs::SigillessAlias,
        MetaNs::SigillessReadonly,
        MetaNs::Type,
        MetaNs::HashKeyType,
        MetaNs::State,
        MetaNs::Bound,
        MetaNs::BoundIndex,
        MetaNs::ShapedArrayDims,
        MetaNs::AtomicArr,
        MetaNs::AtomicHash,
        MetaNs::CallableId,
        MetaNs::ArrayShare,
        MetaNs::AtomicName,
        MetaNs::AtomicValue,
        MetaNs::BoundArrayLen,
        MetaNs::BoundArraySlice,
        MetaNs::BoundDecont,
        MetaNs::Compunit,
        MetaNs::ConstantVar,
        MetaNs::DeepReadonly,
        MetaNs::DeletedIndex,
        MetaNs::ElemShare,
        MetaNs::EvalRole,
        MetaNs::FfState,
        MetaNs::GatherSelfRef,
        MetaNs::InlinePackageProto,
        MetaNs::InlinePackageSub,
        MetaNs::ScalarBindNoContainer,
        MetaNs::SharedState,
        MetaNs::MethodValue,
        MetaNs::Outer,
        MetaNs::PredictiveSeqIter,
        MetaNs::RepoFs,
        MetaNs::RoIndex,
        MetaNs::SharedDirty,
        MetaNs::VarMeta,
        MetaNs::VarSourceName,
        MetaNs::AttrAlias,
        MetaNs::Role,
        MetaNs::RoleSeq,
        MetaNs::RoleId,
        MetaNs::RoleTypeargs,
        MetaNs::RoleParam,
        MetaNs::RoleGroup,
        MetaNs::RoleHides,
        MetaNs::Attr,
        MetaNs::AttrTrait,
        MetaNs::TypeCaptureBound,
    ];

    /// The literal key prefix, including whatever separator the namespace puts
    /// between it and the name — `::` for an env key, a bare `__` for the mixin
    /// registry's keys.
    ///
    /// This is the ONLY place these strings are spelled out. `Symbol`'s own
    /// [`TYPE_META_PREFIX`](crate::symbol::TYPE_META_PREFIX) is reused rather
    /// than respelled, because `Symbol::type_meta_subject` parses keys back
    /// with it and the two must not drift.
    pub(crate) const fn prefix(self) -> &'static str {
        match self {
            MetaNs::SigillessAlias => "__mutsu_sigilless_alias::",
            MetaNs::SigillessReadonly => "__mutsu_sigilless_readonly::",
            MetaNs::Type => crate::symbol::TYPE_META_PREFIX,
            MetaNs::HashKeyType => "__mutsu_hash_key_type::",
            MetaNs::State => "__mutsu_state_key::",
            MetaNs::Bound => "__mutsu_bound::",
            MetaNs::BoundIndex => "__mutsu_bound_index::",
            MetaNs::ShapedArrayDims => "__mutsu_shaped_array_dims::",
            MetaNs::AtomicArr => "__mutsu_atomic_arr::",
            MetaNs::AtomicHash => "__mutsu_atomic_hash::",
            MetaNs::CallableId => "__mutsu_callable_id::",
            MetaNs::ArrayShare => "__mutsu_array_share::",
            MetaNs::AtomicName => "__mutsu_atomic_name::",
            MetaNs::AtomicValue => "__mutsu_atomic_value::",
            MetaNs::BoundArrayLen => "__mutsu_bound_array_len::",
            MetaNs::BoundArraySlice => "__mutsu_bound_array_slice::",
            MetaNs::BoundDecont => "__mutsu_bound_decont::",
            MetaNs::Compunit => "__mutsu_compunit::",
            MetaNs::ConstantVar => "__mutsu_constant_var::",
            MetaNs::DeepReadonly => "__mutsu_deep_readonly::",
            MetaNs::DeletedIndex => "__mutsu_deleted_index::",
            MetaNs::ElemShare => "__mutsu_elem_share::",
            MetaNs::EvalRole => "__mutsu_eval_role::",
            MetaNs::FfState => "__mutsu_ff_state::",
            MetaNs::GatherSelfRef => "__mutsu_gather_self_ref::",
            MetaNs::InlinePackageProto => "__mutsu_inline_package_proto_preregistered::",
            MetaNs::InlinePackageSub => "__mutsu_inline_package_sub_preregistered::",
            MetaNs::ScalarBindNoContainer => "__mutsu_scalar_bind_no_container::",
            MetaNs::SharedState => "__mutsu_shared_state::",
            MetaNs::MethodValue => "__mutsu_method_value::",
            MetaNs::Outer => "__mutsu_outer::",
            MetaNs::PredictiveSeqIter => "__mutsu_predictive_seq_iter::",
            MetaNs::RepoFs => "__mutsu_repo_fs::",
            MetaNs::RoIndex => "__mutsu_ro_index::",
            MetaNs::SharedDirty => "__mutsu_shared_dirty::",
            MetaNs::VarMeta => "__mutsu_var_meta::",
            MetaNs::VarSourceName => "__mutsu_var_source_name::",
            MetaNs::AttrAlias => "__mutsu_attr_alias::",
            MetaNs::Role => "__mutsu_role__",
            MetaNs::RoleSeq => "__mutsu_role_seq__",
            MetaNs::RoleId => "__mutsu_role_id__",
            MetaNs::RoleTypeargs => "__mutsu_role_typeargs__",
            MetaNs::RoleParam => "__mutsu_role_param__",
            MetaNs::RoleGroup => "__mutsu_role_group__",
            MetaNs::RoleHides => "__mutsu_role_hides__",
            MetaNs::Attr => "__mutsu_attr__",
            MetaNs::AttrTrait => "__mutsu_attr_trait__",
            MetaNs::TypeCaptureBound => "__mutsu_type_capture_bound__",
        }
    }

    /// What [`MetaNs::key_pair`] joins this namespace's two halves with.
    ///
    /// `::` everywhere except [`MetaNs::AttrTrait`], whose keys were written
    /// with `!` — the sigil an attribute is declared with — long before this
    /// type existed. Spelling it here rather than at the call site is the point:
    /// the separator is part of the key, and a key that does not match what is
    /// already sitting in a map is not an error, just metadata never found again.
    pub(crate) const fn pair_sep(self) -> &'static str {
        match self {
            MetaNs::AttrTrait => "!",
            _ => "::",
        }
    }

    /// The atomic lane namespace for a container: `%h`'s if `hash_lane`, `@a`'s
    /// otherwise.
    pub(crate) const fn atomic_lane(hash_lane: bool) -> MetaNs {
        if hash_lane {
            MetaNs::AtomicHash
        } else {
            MetaNs::AtomicArr
        }
    }

    /// This namespace's env key for `name`, as a pre-interned [`Symbol`].
    ///
    /// Memoized per `(namespace, name)` in one thread-local table, so the
    /// `format!` and the string hash behind `Symbol::intern` happen once per
    /// pair for the life of the thread. Callers on a hot path should hold the
    /// name as a `Symbol` already (a compiled `SetLocal` slot, a `SetGlobal`
    /// constant, a bound parameter all do) and probe with `Env::get_sym` /
    /// `Env::contains_key_sym`, so that no string is hashed at all.
    pub(crate) fn key(self, name: Symbol) -> Symbol {
        thread_local! {
            static KEYS: std::cell::RefCell<rustc_hash::FxHashMap<(MetaNs, Symbol), Symbol>> =
                std::cell::RefCell::new(rustc_hash::FxHashMap::default());
        }
        let pair = (self, name);
        if let Some(sym) = KEYS.with(|c| c.borrow().get(&pair).copied()) {
            return sym;
        }
        let prefix = self.prefix();
        let sym = name.with_str(|n| Symbol::intern(&format!("{prefix}{n}")));
        KEYS.with(|c| {
            c.borrow_mut().insert(pair, sym);
        });
        sym
    }

    /// This namespace's env key for a two-part name (`<outer>::<inner>`), as a
    /// pre-interned [`Symbol`].
    ///
    /// Only [`MetaNs::CallableId`] is shaped this way: a routine is identified
    /// by its package *and* its name. Memoized per `(namespace, outer, inner)`
    /// exactly like [`Self::key`] — every named compiled call probes this key on
    /// entry, and building it by hand cost a `format!` of a ~40-byte string plus
    /// a hash of those bytes to intern it, per call, for a mapping fixed for the
    /// life of the routine (#7573).
    pub(crate) fn key_pair(self, outer: Symbol, inner: Symbol) -> Symbol {
        thread_local! {
            static PAIR_KEYS: std::cell::RefCell<
                rustc_hash::FxHashMap<(MetaNs, Symbol, Symbol), Symbol>,
            > = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
        }
        let triple = (self, outer, inner);
        if let Some(sym) = PAIR_KEYS.with(|c| c.borrow().get(&triple).copied()) {
            return sym;
        }
        let prefix = self.prefix();
        let sep = self.pair_sep();
        let sym =
            outer.with_str(|o| inner.with_str(|i| Symbol::intern(&format!("{prefix}{o}{sep}{i}"))));
        PAIR_KEYS.with(|c| {
            c.borrow_mut().insert(triple, sym);
        });
        sym
    }

    /// [`Self::key_pair`] for a caller that has both halves as `&str`.
    pub(crate) fn key_pair_for_strs(self, outer: &str, inner: &str) -> Symbol {
        self.key_pair(Symbol::intern(outer), Symbol::intern(inner))
    }

    /// [`Self::key`] for a caller that only has the name as a `&str`.
    ///
    /// Interns `name` before the memo lookup, so it costs a string hash on
    /// every call and is the wrong entry point for a hot path — reach for it
    /// only where the caller genuinely has no `Symbol`.
    pub(crate) fn key_for_str(self, name: &str) -> Symbol {
        self.key(Symbol::intern(name))
    }

    /// [`Self::key_for_str`] as a `&'static str`, for the sites that hand the
    /// key to a by-name API (the shared store is keyed by `&str`, not by
    /// `Symbol`).
    ///
    /// Still worth going through the memo: resolving an interned `Symbol` back
    /// to its `&'static str` is a thread-local array read, where the `format!`
    /// it replaces was a heap allocation, the whole `core::fmt` machinery and a
    /// matching free on every access.
    pub(crate) fn str_key_for_str(self, name: impl AsRef<str>) -> &'static str {
        self.key_for_str(name.as_ref()).as_str()
    }

    /// This namespace's key for a numeric id, as an owned `String`.
    ///
    /// [`MetaNs::AtomicValue`] and [`MetaNs::PredictiveSeqIter`] are keyed by an
    /// identity rather than by a variable name, and an identity is fresh every
    /// time: memoizing one would be a map that only ever grows, which is a leak
    /// dressed up as a cache. So these deliberately rebuild the string — the
    /// point of routing them through here is that the prefix is still spelled in
    /// exactly one place, not that the construction is saved.
    /// [`Self::owned_key_from_parts`] is the same bargain for the namespaces
    /// whose identity is not a single number.
    pub(crate) fn key_for_id(self, id: u64) -> String {
        let prefix = self.prefix();
        format!("{prefix}{id}")
    }

    /// This namespace's key for an arbitrary number of parts, joined with
    /// [`Self::pair_sep`], as an owned `String`.
    ///
    /// For the namespaces whose key is neither one name nor two: a
    /// [`MetaNs::InlinePackageSub`] marker carries package, name *and* the
    /// declaration site's fingerprint, and the same call with a trailing empty
    /// part produces the `…::<package>::<name>::` prefix that the proto
    /// registration scans the env by. Not memoized — a fingerprint, a
    /// flip-flop's dynamic scope and a [`MetaNs::SharedState`] key's routine
    /// clone id all vary per execution rather than per program text, so a memo
    /// would only ever grow. Single-part callers reach for it too, for exactly
    /// that reason.
    pub(crate) fn owned_key_from_parts(self, parts: &[&str]) -> String {
        let mut key = String::from(self.prefix());
        for (i, part) in parts.iter().enumerate() {
            if i > 0 {
                key.push_str(self.pair_sep());
            }
            key.push_str(part);
        }
        key
    }

    /// [`Self::str_key_for_str`] as an owned `String`, for the sites that have
    /// to hand the key to a `HashMap<String, _>::insert`.
    ///
    /// The mixin registry ([`MetaNs::Role`] and friends) is keyed by `String`,
    /// so an insert there needs an allocation no matter what. Going through the
    /// memo still pays: this is one `copy_from_slice` of a length the allocator
    /// is told up front, where the `format!` it replaces ran the whole
    /// `core::fmt` machinery. Reads of that same map want
    /// [`Self::str_key_for_str`] and allocate nothing at all.
    pub(crate) fn owned_key_for_str(self, name: impl AsRef<str>) -> String {
        self.str_key_for_str(name.as_ref()).to_string()
    }

    /// [`Self::owned_key_for_str`] for a caller that already holds the name as
    /// a [`Symbol`] — the memo lookup then costs no string hash at all.
    pub(crate) fn owned_key(self, name: Symbol) -> String {
        self.str_key(name).to_string()
    }

    /// [`Self::str_key_for_str`] for a caller that already holds the name as a
    /// [`Symbol`]. The cheapest form there is: an integer-keyed memo probe and
    /// a thread-local array read, with no string touched on either side.
    pub(crate) fn str_key(self, name: Symbol) -> &'static str {
        self.key(name).as_str()
    }

    /// [`Self::key_pair_for_strs`] as an owned `String`, for a two-part key
    /// going into a `HashMap<String, _>`.
    pub(crate) fn owned_key_pair_for_strs(self, outer: &str, inner: &str) -> String {
        self.key_pair_for_strs(outer, inner).as_str().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_key_is_the_prefix_joined_to_the_name() {
        let key = MetaNs::SigillessAlias.key(Symbol::intern("@a"));
        assert_eq!(key.as_str(), "__mutsu_sigilless_alias::@a");
        assert_eq!(
            MetaNs::Type.key_for_str("$x").as_str(),
            "__mutsu_type::$x",
            "the Type namespace must agree with symbol::TYPE_META_PREFIX, which \
             Symbol::type_meta_subject parses keys back with"
        );
    }

    #[test]
    fn the_memo_returns_the_same_symbol_and_namespaces_do_not_collide() {
        let name = Symbol::intern("%h");
        assert_eq!(MetaNs::Bound.key(name), MetaNs::Bound.key(name));
        assert_ne!(
            MetaNs::Bound.key(name),
            MetaNs::BoundIndex.key(name),
            "the memo is keyed by (namespace, name); a name-only key would \
             hand `__mutsu_bound_index::` the `__mutsu_bound::` entry"
        );
    }

    /// The exact key every namespace produces, spelled out.
    ///
    /// A prefix typo is invisible at the type level and nearly invisible at
    /// runtime: the writer stores under one key, the reader probes another, and
    /// the metadata is simply never found — no error, just a lost constraint or
    /// a lost alias. Each line below is the spelling that the hand-built
    /// `format!` sites this type replaced produced (#8087), so the table cannot
    /// drift away from the keys already sitting in an env.
    #[test]
    fn every_namespace_spells_its_key_exactly_as_the_format_sites_did() {
        let n = Symbol::intern("@a");
        let spellings = [
            (MetaNs::SigillessAlias, "__mutsu_sigilless_alias::@a"),
            (MetaNs::SigillessReadonly, "__mutsu_sigilless_readonly::@a"),
            (MetaNs::Type, "__mutsu_type::@a"),
            (MetaNs::HashKeyType, "__mutsu_hash_key_type::@a"),
            (MetaNs::State, "__mutsu_state_key::@a"),
            (MetaNs::Bound, "__mutsu_bound::@a"),
            (MetaNs::BoundIndex, "__mutsu_bound_index::@a"),
            (MetaNs::ShapedArrayDims, "__mutsu_shaped_array_dims::@a"),
            (MetaNs::AtomicArr, "__mutsu_atomic_arr::@a"),
            (MetaNs::AtomicHash, "__mutsu_atomic_hash::@a"),
            (MetaNs::ArrayShare, "__mutsu_array_share::@a"),
            (MetaNs::AtomicName, "__mutsu_atomic_name::@a"),
            (MetaNs::AtomicValue, "__mutsu_atomic_value::@a"),
            (MetaNs::BoundArrayLen, "__mutsu_bound_array_len::@a"),
            (MetaNs::BoundArraySlice, "__mutsu_bound_array_slice::@a"),
            (MetaNs::BoundDecont, "__mutsu_bound_decont::@a"),
            (MetaNs::ConstantVar, "__mutsu_constant_var::@a"),
            (MetaNs::DeepReadonly, "__mutsu_deep_readonly::@a"),
            (MetaNs::DeletedIndex, "__mutsu_deleted_index::@a"),
            (MetaNs::ElemShare, "__mutsu_elem_share::@a"),
            (MetaNs::EvalRole, "__mutsu_eval_role::@a"),
            (MetaNs::GatherSelfRef, "__mutsu_gather_self_ref::@a"),
            (MetaNs::MethodValue, "__mutsu_method_value::@a"),
            (
                MetaNs::ScalarBindNoContainer,
                "__mutsu_scalar_bind_no_container::@a",
            ),
            (MetaNs::SharedState, "__mutsu_shared_state::@a"),
            (MetaNs::Outer, "__mutsu_outer::@a"),
            (MetaNs::PredictiveSeqIter, "__mutsu_predictive_seq_iter::@a"),
            (MetaNs::RepoFs, "__mutsu_repo_fs::@a"),
            (MetaNs::RoIndex, "__mutsu_ro_index::@a"),
            (MetaNs::SharedDirty, "__mutsu_shared_dirty::@a"),
            (MetaNs::VarMeta, "__mutsu_var_meta::@a"),
            (MetaNs::VarSourceName, "__mutsu_var_source_name::@a"),
            (MetaNs::AttrAlias, "__mutsu_attr_alias::@a"),
            (MetaNs::Role, "__mutsu_role__@a"),
            (MetaNs::RoleSeq, "__mutsu_role_seq__@a"),
            (MetaNs::RoleId, "__mutsu_role_id__@a"),
            (MetaNs::RoleTypeargs, "__mutsu_role_typeargs__@a"),
            (MetaNs::RoleParam, "__mutsu_role_param__@a"),
            (MetaNs::RoleGroup, "__mutsu_role_group__@a"),
            (MetaNs::RoleHides, "__mutsu_role_hides__@a"),
            (MetaNs::Attr, "__mutsu_attr__@a"),
            (MetaNs::TypeCaptureBound, "__mutsu_type_capture_bound__@a"),
        ];
        for (ns, expected) in spellings {
            assert_eq!(ns.key(n).as_str(), expected, "{ns:?} key spelling");
        }
        let pairs = [
            (
                MetaNs::CallableId,
                "__mutsu_callable_id::GLOBAL::f",
                "::" as &str,
            ),
            (MetaNs::Compunit, "__mutsu_compunit::GLOBAL::f", "::"),
            (MetaNs::FfState, "__mutsu_ff_state::GLOBAL::f", "::"),
            (
                MetaNs::InlinePackageProto,
                "__mutsu_inline_package_proto_preregistered::GLOBAL::f",
                "::",
            ),
            (MetaNs::AttrTrait, "__mutsu_attr_trait__GLOBAL!f", "!"),
        ];
        for (ns, expected, sep) in pairs {
            assert_eq!(
                ns.key_pair_for_strs("GLOBAL", "f").as_str(),
                expected,
                "{ns:?} pair key spelling"
            );
            assert_eq!(ns.pair_sep(), sep, "{ns:?} pair separator");
        }
        // The one namespace with three parts, and the only one whose key is
        // also used as a PREFIX to scan the env by -- an empty trailing part is
        // how the proto registration asks for `…::<package>::<name>::`, so the
        // trailing separator is part of what it means and must survive.
        let parts = [(
            MetaNs::InlinePackageSub,
            "__mutsu_inline_package_sub_preregistered::GLOBAL::f::17",
            "__mutsu_inline_package_sub_preregistered::GLOBAL::f::",
        )];
        for (ns, expected, prefix_form) in parts {
            assert_eq!(
                ns.owned_key_from_parts(&["GLOBAL", "f", "17"]),
                expected,
                "{ns:?} three-part key spelling"
            );
            assert_eq!(
                ns.owned_key_from_parts(&["GLOBAL", "f", ""]),
                prefix_form,
                "{ns:?} prefix-scan form keeps its trailing separator"
            );
        }
        // Every one-part namespace is covered above, and every two-part one
        // just below it -- so adding a variant without pinning its spelling
        // fails here rather than silently writing a key nothing reads.
        assert_eq!(
            spellings.len() + pairs.len() + parts.len(),
            MetaNs::ALL.len(),
            "a namespace was added to MetaNs::ALL without a pinned key spelling"
        );
    }

    #[test]
    fn an_owned_key_matches_the_borrowed_one() {
        assert_eq!(
            MetaNs::Role.owned_key_for_str("Stringy"),
            MetaNs::Role.str_key_for_str("Stringy"),
            "the mixin registry is String-keyed; its insert and its lookup must \
             agree on the spelling"
        );
        assert_eq!(
            MetaNs::AttrTrait.owned_key_pair_for_strs("C", "$!x"),
            "__mutsu_attr_trait__C!$!x"
        );
    }

    #[test]
    fn a_pair_key_is_memoized_per_both_halves() {
        let (a, b, f) = (
            Symbol::intern("A"),
            Symbol::intern("B"),
            Symbol::intern("f"),
        );
        assert_eq!(
            MetaNs::CallableId.key_pair(a, f),
            MetaNs::CallableId.key_pair(a, f)
        );
        assert_ne!(
            MetaNs::CallableId.key_pair(a, f),
            MetaNs::CallableId.key_pair(b, f),
            "two packages' same-named routines must not share one id key"
        );
        // `A::x` + `y` and `A` + `x::y` both flatten to the same string; the
        // memo must still hold them apart by the pair it was keyed with, and
        // the flattened key is what the env sees either way.
        assert_eq!(
            MetaNs::CallableId.key_pair_for_strs("A::x", "y"),
            MetaNs::CallableId.key_pair_for_strs("A", "x::y"),
            "the key is the flattened string, so an ambiguous split collides \
             in the env by construction -- pinned so a future namespace is not \
             given a separator that makes that ambiguity load-bearing"
        );
    }

    #[test]
    fn the_atomic_lane_selector_picks_the_matching_namespace() {
        assert_eq!(MetaNs::atomic_lane(false), MetaNs::AtomicArr);
        assert_eq!(MetaNs::atomic_lane(true), MetaNs::AtomicHash);
        assert_eq!(
            MetaNs::AtomicArr.str_key_for_str("@a"),
            "__mutsu_atomic_arr::@a",
            "the &str form must resolve to the same spelling the Symbol does"
        );
    }

    #[test]
    fn every_prefix_is_distinct_and_well_formed() {
        let mut seen = std::collections::HashSet::new();
        for &ns in MetaNs::ALL {
            let p = ns.prefix();
            assert!(p.starts_with("__mutsu_"), "{ns:?} prefix is not internal");
            assert!(
                p.ends_with("::") || p.ends_with("__"),
                "{ns:?} prefix must end with its own separator (`::` for an env \
                 key, `__` for a mixin-registry key), or the name runs straight \
                 into the namespace"
            );
            assert!(seen.insert(p), "{ns:?} duplicates another prefix");
        }
        // No prefix may be a prefix of another: `__mutsu_bound::` vs
        // `__mutsu_bound_index::`, `__mutsu_role__` vs `__mutsu_role_seq__`,
        // `__mutsu_attr__` vs `__mutsu_attr_trait__` all come within one
        // character of colliding. `MixinOverrides::seed_missing_attributes`
        // really does enumerate the registry with `strip_prefix`, so an
        // ambiguous table there would hand one namespace another's entries.
        for &a in MetaNs::ALL {
            for &b in MetaNs::ALL {
                if a != b {
                    assert!(
                        !b.prefix().starts_with(a.prefix()),
                        "{b:?}'s prefix starts with {a:?}'s; a prefix probe \
                         cannot tell them apart"
                    );
                }
            }
        }
    }
}

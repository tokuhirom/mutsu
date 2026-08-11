//! ADR-0019 Phase E box E4a: the sequence builder (user candidates only, shadow mode).
//!
//! [`Interpreter::resolve_sequence`] walks an E1 [`TypeId`] MRO chain and collects
//! every visible user-declared method candidate for a name into a flat
//! [`ResolvedSequence`] — the "shape-independent candidate universe" of design
//! decision 4 in `todo/deep/adr0019-e2-e4-resolver-core.md`. Nothing in the VM or
//! the interpreter's real dispatch reads a sequence today: E4a only builds one
//! beside the existing resolver, at the two `resolve_method_cached` boundaries, and
//! compares the winner ([`Interpreter::pick_method_winner`], extracted from
//! `resolve_method_with_owner_impl` unchanged) against the real resolution under
//! `MUTSU_VM_STATS` counters (`resolver_shadow_checks`/`_mismatches`).
//!
//! **Known, accepted divergence** (not yet modeled — E8's job per the design doc):
//! `resolve_method_with_owner_impl` treats a non-multi method's lookup as independent
//! of whether the call's arguments actually bind it — Raku itself resolves a plain
//! (non-multi) method purely by name; a signature mismatch (an `is rw` param fed a
//! literal, a typed param fed the wrong type) is a bind-time error raised *after*
//! resolution, not a lookup failure that falls through to a differently-shaped
//! candidate. Two concrete instances of that rule, both load-bearing for
//! `resolve_method_with_owner_impl`'s early-stopping MRO walk:
//! - a single visible non-multi candidate is returned even when its own signature
//!   does not match the call (`first_visible_non_multi` in the real resolver);
//! - a non-multi override on a more-derived class hides same-named candidates on
//!   every ancestor the same way, regardless of match, while a pure-submethod level
//!   does not count as such an override.
//!
//! The E4a shadow winner only ranks candidates that already passed
//! `method_args_match_for_invocant`, so it has no notion of either case: it answers
//! `None` where the real resolver still returns the sole non-matching candidate.
//! Confirmed empirically (2026-08-10 sweep, see the landed PR note) — every
//! observed mismatch was exactly this shape (`real=Some(..) shadow=None` for a
//! single-candidate class, e.g. `method assign-rw($a is rw)` called with a
//! literal). Expected on the sweep and bucketed (like E1a's ledger) rather than
//! blocking the box; unifying it is E8's job (candidates carry `level`/`stored_idx`
//! so this exact rule becomes representable in the sequence itself).

use super::*;
use crate::builtins::native_method_row::{NativeArityMask, native_row_servable};
use crate::type_id::TypeId;
use std::sync::Arc;

/// The E4b-local subset of call-shape facts design decision 4's `Native`
/// candidate needs to decide whether a native row is actually reachable for
/// one specific call: the call's own arity, and whether the receiver is a
/// concrete value (`DEFINITE`) rather than a bare type object. This is
/// deliberately smaller than the design doc's future E3 cache-key `CallShape`
/// (`{ arity_bucket, has_named }`, `todo/deep/adr0019-e2-e4-resolver-core.md`)
/// — see the step-4 scoping note in
/// `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md` for
/// why the full shape is not needed here.
#[derive(Clone, Copy)]
pub(crate) struct NativeCallShape {
    pub(crate) arity: NativeArityMask,
    pub(crate) definite: bool,
}

impl NativeCallShape {
    pub(crate) fn new(arg_count: usize, definite: bool) -> Self {
        Self {
            arity: NativeArityMask::for_arity(arg_count),
            definite,
        }
    }
}

/// One candidate in a [`ResolvedSequence`]. E4a only ever constructed `User`;
/// E4b adds the NativeCall-binding and native-row-catalog kinds (design
/// decision 4).
#[derive(Clone)]
pub(crate) enum ResolvedCandidate {
    /// A user-declared method, at its MRO level, in the class's stored
    /// declaration order.
    User { owner: TypeId, def: Arc<MethodDef> },
    /// A `ClassDef::native_methods` binding — an `is native(&sym)` NativeCall
    /// trait, or one of the handful of built-in classes whose getters are
    /// implemented by dedicated `native_io_*` dispatch helpers
    /// (`Interpreter::hardcoded_native_method`). This is the E4b decomposition
    /// note's "category 2": a third candidate kind, distinct from both `User`
    /// and `Native` below — see
    /// `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`.
    /// At most one appears per sequence: `is_native_method` is a boolean "does
    /// any MRO level bind this name", not a per-level fact, so the sequence
    /// records only the first (most-derived) owner that has it.
    NativeCallBinding { owner: TypeId },
    /// A `native_method_{0,1,2}arg` catalog row (E2's `native_method_row`
    /// table) that is actually reachable for the call's shape — design
    /// decision 4's `Native` variant. At most one appears per sequence: rows
    /// are name-based (not per-level user overloads), so the first MRO level
    /// (including its `canonical_builtin_owner` fold) with a servable row
    /// wins, mirroring how the arity cascades themselves are name-dispatched
    /// rather than per-level.
    Native { owner: TypeId },
}

/// The shape-independent ordered candidate universe for one `(receiver chain,
/// method name)` pair. See the module doc for what E4a does and does not yet
/// model.
#[derive(Clone)]
pub(crate) struct ResolvedSequence {
    /// The registry's `method_generation` at build time — a debug/comparison
    /// aid for E3's future cache, not consulted by anything in E4a.
    #[allow(dead_code)]
    pub(crate) generation: u64,
    pub(crate) candidates: Vec<ResolvedCandidate>,
}

/// Whether `value` is a concrete instance rather than a bare type object —
/// the same "DEFINITE" primitive `dispatch_core_coerce.rs`'s `.DEFINITE` arm
/// implements, needed here to decide whether a `Native` candidate's row
/// requires [`crate::builtins::native_method_row::NativeRowFlags::TYPE_OBJECT_OK`].
fn value_is_definite(value: &Value) -> bool {
    match value.view() {
        ValueView::Nil | ValueView::Package(_) | ValueView::CustomType(..) => false,
        ValueView::Slip(items) if items.is_empty() => false,
        _ => true,
    }
}

impl Interpreter {
    /// Build the ordered user-candidate sequence for `chain` (an E1 TypeId MRO,
    /// most-derived first) and `name`: every non-private, non-submethod-shadowed
    /// candidate at every level, in stored declaration order. Mirrors the
    /// membership rules `resolve_method_with_owner_impl` applies per candidate
    /// (`is_private` skip; `is_my` skip when the level is an ancestor) but not its
    /// early-stopping MRO-walk control flow — see the module doc.
    pub(crate) fn resolve_sequence(
        &mut self,
        chain: &[TypeId],
        name: Symbol,
        native_shape: NativeCallShape,
    ) -> ResolvedSequence {
        let generation = self.registry().method_generation;
        let mut candidates = Vec::new();
        let mut native_binding_found = false;
        let mut native_row_found = false;
        for (level, owner) in chain.iter().enumerate() {
            let is_ancestor = level > 0;
            let owner_str = owner.as_str();
            if let Some(overloads) = self
                .registry()
                .user_method_overloads(owner_str, name.as_str())
            {
                for def in overloads {
                    if def.is_private || (def.is_my && is_ancestor) {
                        continue;
                    }
                    candidates.push(ResolvedCandidate::User {
                        owner: *owner,
                        def: Arc::new(def),
                    });
                }
            }
            // `hardcoded_native_method` only ever fires for the receiver's own
            // (most-derived) class name, mirroring `is_native_method` — it is
            // never checked against an ancestor level.
            if !native_binding_found {
                let hardcoded =
                    level == 0 && Interpreter::hardcoded_native_method(owner_str, name.as_str());
                let registered = self
                    .registry()
                    .classes
                    .get(owner_str)
                    .is_some_and(|cd| cd.native_methods.contains(name.as_str()));
                if hardcoded || registered {
                    candidates.push(ResolvedCandidate::NativeCallBinding { owner: *owner });
                    native_binding_found = true;
                }
            }
            // Rows are name-based, not per-level user overloads: the arity
            // cascades dispatch by name alone, so the first MRO level whose
            // (possibly folded) owner has a servable row wins, same as
            // `NativeCallBinding` above.
            if !native_row_found
                && native_row_servable(
                    owner_str,
                    name.as_str(),
                    native_shape.arity,
                    native_shape.definite,
                )
            {
                candidates.push(ResolvedCandidate::Native { owner: *owner });
                native_row_found = true;
            }
        }
        ResolvedSequence {
            generation,
            candidates,
        }
    }

    /// ADR-0019 E4a shadow probe (`MUTSU_VM_STATS`-gated, a no-op otherwise):
    /// build the sequence for `invocant`/`name`, rank it with
    /// [`Self::pick_method_winner`] against `arg_values`, and compare the winner to
    /// `real` — the answer `resolve_method_cached`'s two resolution boundaries just
    /// computed via the existing `resolve_method_with_owner_impl` path. Purely
    /// observational: `real` continues to drive dispatch unchanged.
    ///
    /// Two care points, both required to keep this a true zero-behavior-change
    /// probe:
    /// - `self.dispatch_ambiguous` is saved and restored around the shadow
    ///   ranking, since [`Self::pick_method_winner`] can set it — without the
    ///   save/restore, a shadow-only ambiguity would silently overwrite the real
    ///   resolution's (correct) flag, which `resolve_method_cached`'s caller reads
    ///   immediately afterward.
    /// - candidates whose signature carries a `where` clause are skipped
    ///   entirely (no `method_args_match_for_invocant` call at all): a `where`
    ///   clause is user code, and its dynamic-variable (`$*x`) writes are a real,
    ///   deliberately-preserved side effect (see
    ///   `Interpreter::restore_env_preserving_dynamics`) — running the same match
    ///   twice would duplicate that side effect.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn shadow_check_resolver(
        &mut self,
        site: &'static str,
        class_name: &str,
        method: &str,
        method_sym: Symbol,
        arg_values: &[Value],
        invocant: &Value,
        real: Option<&(Symbol, MethodDef)>,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let saved_ambiguous = self.dispatch_ambiguous;
        let chain = self.dispatch_mro(invocant);
        let native_shape = NativeCallShape::new(arg_values.len(), value_is_definite(invocant));
        let seq = self.resolve_sequence(&chain, method_sym, native_shape);
        let has_where_candidate = seq.candidates.iter().any(|c| {
            let ResolvedCandidate::User { def, .. } = c else {
                return false;
            };
            def.param_defs.iter().any(|p| p.where_constraint.is_some())
        });
        if has_where_candidate {
            self.dispatch_ambiguous = saved_ambiguous;
            return;
        }
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro: Vec<Symbol> = chain.iter().map(|t| t.symbol()).collect();
        let mut matched: Vec<(Symbol, MethodDef)> = Vec::new();
        for c in &seq.candidates {
            let ResolvedCandidate::User { owner, def } = c else {
                continue;
            };
            if self.method_args_match_for_invocant(
                class_name,
                def,
                arg_values,
                role_bindings.as_ref(),
                Some(invocant),
            ) {
                matched.push((owner.symbol(), (**def).clone()));
            }
        }
        let shadow = self.pick_method_winner(&mro, arg_values, Some(invocant), matched);
        self.dispatch_ambiguous = saved_ambiguous;
        let matched_ok = match (real, shadow.as_ref()) {
            (None, None) => true,
            (Some((ro, rd)), Some((so, sd))) => ro == so && Arc::ptr_eq(&rd.body, &sd.body),
            _ => false,
        };
        crate::vm::vm_stats::record_resolver_shadow_check(site, matched_ok, || {
            format!(
                "class={class_name} method={method} real={:?} shadow={:?}",
                real.map(|(o, _)| o.as_str()),
                shadow.as_ref().map(|(o, _)| o.as_str()),
            )
        });
    }

    /// ADR-0019 E4b step 4/9 shadow probe (`MUTSU_VM_STATS`-gated, a no-op
    /// otherwise): does `resolve_sequence`'s new `Native` candidate agree
    /// with whether the pure arity cascade actually served this call?
    /// `real_served` must be a result the caller already computed by
    /// actually invoking `native_method_{0,1,2}arg` (`call_method_with_values`
    /// only calls this when `!bypass_native_fastpath`, i.e. the cascade was
    /// genuinely consulted) — this function never invokes the cascade
    /// itself, so it carries no double-invocation side-effect risk even for
    /// a mutating row.
    pub(crate) fn shadow_check_native_row_candidate(
        &mut self,
        target: &Value,
        method: &str,
        method_sym: Symbol,
        arg_count: usize,
        real_served: bool,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let chain = self.dispatch_mro(target);
        let native_shape = NativeCallShape::new(arg_count, value_is_definite(target));
        let seq = self.resolve_sequence(&chain, method_sym, native_shape);
        let native_row_owner = seq.candidates.iter().find_map(|c| match c {
            ResolvedCandidate::Native { owner } => Some(owner.as_str()),
            ResolvedCandidate::User { .. } | ResolvedCandidate::NativeCallBinding { .. } => None,
        });
        let shadow_served = native_row_owner.is_some();
        crate::vm::vm_stats::record_native_row_shadow_check(real_served == shadow_served, || {
            format!(
                "method={method} arity={arg_count} real={real_served} shadow={shadow_served} native_row_owner={native_row_owner:?}"
            )
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn interp() -> Interpreter {
        Interpreter::new()
    }

    #[test]
    fn resolve_sequence_collects_candidates_in_mro_order() {
        let mut i = interp();
        i.run("class Base { method greet { 'base' } }\nclass Child is Base { method greet { 'child' } }")
            .unwrap();
        let chain = vec![TypeId::intern("Child"), TypeId::intern("Base")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("greet"), default_shape());
        let owners: Vec<&str> =
            seq.candidates
                .iter()
                .filter_map(|c| match c {
                    ResolvedCandidate::User { owner, .. } => Some(owner.as_str()),
                    ResolvedCandidate::NativeCallBinding { .. }
                    | ResolvedCandidate::Native { .. } => None,
                })
                .collect();
        assert_eq!(owners, vec!["Child", "Base"]);
    }

    #[test]
    fn resolve_sequence_skips_a_submethod_on_an_ancestor_level() {
        let mut i = interp();
        i.run("class Base { submethod only-base { } }\nclass Child is Base { }")
            .unwrap();
        let chain = vec![TypeId::intern("Child"), TypeId::intern("Base")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("only-base"), default_shape());
        assert!(
            seq.candidates.is_empty(),
            "a submethod on an ancestor level must not appear in a descendant's sequence"
        );
    }

    #[test]
    fn resolve_sequence_finds_a_submethod_at_its_own_level() {
        let mut i = interp();
        i.run("class Base { submethod only-base { } }\nclass Child is Base { }")
            .unwrap();
        let chain = vec![TypeId::intern("Base")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("only-base"), default_shape());
        assert_eq!(seq.candidates.len(), 1);
    }

    #[test]
    fn resolve_sequence_is_empty_for_an_unknown_method() {
        let mut i = interp();
        i.run("class Base { }").unwrap();
        let chain = vec![TypeId::intern("Base")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("nope"), default_shape());
        assert!(seq.candidates.is_empty());
    }

    /// ADR-0019 E4b step 3: a pure `is native(&sym)` binding with no matching
    /// accessor (the `Supply.tap` shape the step-1 shadow sweep found
    /// invisible to `resolve_user_method_or_accessor`) must now surface as
    /// its own candidate.
    #[test]
    fn resolve_sequence_finds_a_registry_native_call_binding() {
        let mut i = interp();
        assert!(i.is_native_method("Supply", "tap"));
        let chain = vec![TypeId::intern("Supply")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("tap"), default_shape());
        assert!(
            seq.candidates.iter().any(
                |c| matches!(c, ResolvedCandidate::NativeCallBinding { owner }
                    if owner.as_str() == "Supply")
            ),
            "expected a NativeCallBinding candidate for Supply.tap"
        );
    }

    /// ADR-0019 E4b step 4/9: design decision 4's `Native` variant -- a
    /// catalog row that is actually reachable for the call's shape.
    /// `Str.chars` is a plain `A0`/`TYPE_OBJECT_OK` row.
    #[test]
    fn resolve_sequence_finds_a_servable_native_row() {
        let mut i = interp();
        let chain = vec![TypeId::intern("Str")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("chars"),
            NativeCallShape::new(0, true),
        );
        assert!(
            seq.candidates.iter().any(
                |c| matches!(c, ResolvedCandidate::Native { owner } if owner.as_str() == "Str")
            ),
            "expected a Native candidate for Str.chars at arity 0"
        );
    }

    /// The same row is not offered at an arity the call doesn't have.
    #[test]
    fn resolve_sequence_omits_a_native_row_at_the_wrong_arity() {
        let mut i = interp();
        let chain = vec![TypeId::intern("Str")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("chars"),
            NativeCallShape::new(1, true),
        );
        assert!(
            !seq.candidates
                .iter()
                .any(|c| matches!(c, ResolvedCandidate::Native { .. })),
            "Str.chars is an A0 row and must not surface for a 1-arg call"
        );
    }

    /// The hardcoded native-method table (`IO::Handle`, etc.) only applies at
    /// the receiver's own (most-derived) level, mirroring `is_native_method` —
    /// an ancestor level must not spuriously pick it up.
    #[test]
    fn resolve_sequence_hardcoded_native_binding_is_not_seen_at_an_ancestor_level() {
        let mut i = interp();
        assert!(i.is_native_method("IO::Handle", "chomp"));
        let chain = vec![TypeId::intern("Base"), TypeId::intern("IO::Handle")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("chomp"), default_shape());
        assert!(
            seq.candidates.is_empty(),
            "hardcoded native-method names must not apply to an ancestor level"
        );
    }

    fn default_shape() -> NativeCallShape {
        NativeCallShape::new(0, true)
    }
}

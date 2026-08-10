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
use crate::type_id::TypeId;
use std::sync::Arc;

/// One candidate in a [`ResolvedSequence`]. E4a only ever constructs `User`; the
/// accessor bit and native rows join in E4b (design decision 4).
#[derive(Clone)]
pub(crate) enum ResolvedCandidate {
    /// A user-declared method, at its MRO level, in the class's stored
    /// declaration order.
    User { owner: TypeId, def: Arc<MethodDef> },
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

impl Interpreter {
    /// Build the ordered user-candidate sequence for `chain` (an E1 TypeId MRO,
    /// most-derived first) and `name`: every non-private, non-submethod-shadowed
    /// candidate at every level, in stored declaration order. Mirrors the
    /// membership rules `resolve_method_with_owner_impl` applies per candidate
    /// (`is_private` skip; `is_my` skip when the level is an ancestor) but not its
    /// early-stopping MRO-walk control flow — see the module doc.
    pub(crate) fn resolve_sequence(&mut self, chain: &[TypeId], name: Symbol) -> ResolvedSequence {
        let generation = self.registry().method_generation;
        let mut candidates = Vec::new();
        for (level, owner) in chain.iter().enumerate() {
            let is_ancestor = level > 0;
            let Some(overloads) = self
                .registry()
                .user_method_overloads(owner.as_str(), name.as_str())
            else {
                continue;
            };
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
        let seq = self.resolve_sequence(&chain, method_sym);
        let has_where_candidate = seq.candidates.iter().any(|c| {
            let ResolvedCandidate::User { def, .. } = c;
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
            let ResolvedCandidate::User { owner, def } = c;
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
        let seq = i.resolve_sequence(&chain, Symbol::intern("greet"));
        let owners: Vec<&str> = seq
            .candidates
            .iter()
            .map(|ResolvedCandidate::User { owner, .. }| owner.as_str())
            .collect();
        assert_eq!(owners, vec!["Child", "Base"]);
    }

    #[test]
    fn resolve_sequence_skips_a_submethod_on_an_ancestor_level() {
        let mut i = interp();
        i.run("class Base { submethod only-base { } }\nclass Child is Base { }")
            .unwrap();
        let chain = vec![TypeId::intern("Child"), TypeId::intern("Base")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("only-base"));
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
        let seq = i.resolve_sequence(&chain, Symbol::intern("only-base"));
        assert_eq!(seq.candidates.len(), 1);
    }

    #[test]
    fn resolve_sequence_is_empty_for_an_unknown_method() {
        let mut i = interp();
        i.run("class Base { }").unwrap();
        let chain = vec![TypeId::intern("Base")];
        let seq = i.resolve_sequence(&chain, Symbol::intern("nope"));
        assert!(seq.candidates.is_empty());
    }
}

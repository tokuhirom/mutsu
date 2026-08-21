//! ADR-0019 Phase E box E4a: the sequence builder (user candidates only, shadow mode).
//!
//! [`Interpreter::resolve_sequence`] walks an E1 [`TypeId`] MRO chain and collects
//! every visible user-declared method candidate for a name into a flat
//! [`ResolvedSequence`] — the "shape-independent candidate universe" of design
//! decision 4 in `todo/deep/adr0019-e2-e4-resolver-core.md`. Nothing in the VM or
//! the interpreter's real dispatch reads a sequence today: E4a only builds one
//! beside the existing resolver, at the two `resolve_method_cached` boundaries, and
//! compares the winner against the real resolution under `MUTSU_VM_STATS` counters
//! (`resolver_shadow_checks`/`_mismatches`).
//!
//! **The non-multi early-stopping rule, modeled (ADR-0019 E3).**
//! `resolve_method_with_owner_impl` treats a non-multi method's lookup as independent
//! of whether the call's arguments actually bind it — Raku itself resolves a plain
//! (non-multi) method purely by name; a signature mismatch (an `is rw` param fed a
//! literal, a typed param fed the wrong type) is a bind-time error raised *after*
//! resolution, not a lookup failure that falls through to a differently-shaped
//! candidate. [`Interpreter::pick_method_winner_from_sequence`] reproduces this
//! exactly by walking the sequence's `(level, stored_idx)`-ordered `User` candidates
//! grouped by level: the first level with no multi candidate and no matches
//! accumulated yet from a more-derived level is a *decision level* — it returns the
//! first stored-order match, or (mirroring `first_visible_non_multi`) the first
//! visible candidate at all when none match. A level whose only candidates are
//! ancestor submethods contributes zero entries to the sequence in the first place
//! ([`Interpreter::resolve_sequence`]'s own `is_my && is_ancestor` filter), so it is
//! transparently skipped exactly like `resolve_method_with_owner_impl`'s
//! `submethod_blocks` continue. Once any level contributes a match to the running
//! multi-candidate set, every subsequent level (multi or not) only ever contributes
//! matches to that set — the walk never single-candidate-early-returns again — and
//! the final winner is [`Interpreter::pick_method_winner`]'s existing tie-break
//! ladder, unchanged.
//!
//! **E8a's own accepted divergence, found by the new deferral-list shadow
//! check** ([`Interpreter::shadow_check_deferral_sequence`]), root-caused and
//! fixed by `todo/tickets/e8a-deferral-shadow-sequence-is-role-blind.md`
//! (2026-08-21): [`Self::resolve_sequence`]'s per-level lookup silently
//! omitted every candidate owned by a **role** that had never been *punned*
//! (used as a standalone type via `RoleName.new`), because it always read
//! the plain `Registry::user_method_overloads` — `Registry::method_entries`
//! (the E1/E2 canonical table that reads) deliberately excludes role owners
//! as stated policy (ADR-0019 F4c design note (1); the table is maintained
//! by `registry_method_table.rs`'s per-declaration mutators, not by any
//! writer's early return). The real deferral-list walker,
//! [`Interpreter::resolve_deferral_expansion`] (ADR-0019 E9a,
//! `resolution_deferral.rs`), has no such gap: its per-level lookup,
//! `own_overloads_at_level`, consults
//! `Registry::get_method_overloads_with_role_fallback`, which reads
//! `Registry::roles` directly when the plain table has nothing — so every
//! mismatch the shadow check found (2026-08-12 sweep) had the same shape:
//! the sequence was missing exactly the role-owned candidate the real walker
//! still found (a role's un-flattened method the composing class overrides
//! with its own, or a role-qualified call `self.R::name()`). Full root-cause
//! writeup in `news/2026-08/method-entries-never-covers-unpunned-roles.md`.
//!
//! The fix is the [`RoleFallback`] parameter: [`Self::resolve_sequence`] is
//! *also* the candidate source for live winner selection
//! ([`Interpreter::resolve_via_sequence_cache`]), and ADR-0019 F4a's rule is
//! that winner selection must never consult the role fallback, so widening
//! the lookup unconditionally was not an option. Only
//! [`Interpreter::shadow_check_deferral_sequence`]'s own call site passes
//! [`RoleFallback::Enabled`]; every other caller — winner selection included,
//! whose `resolved_seq_cache` key must keep meaning "no fallback" — passes
//! [`RoleFallback::Disabled`], the original role-blind behavior, unchanged.
//! A second, unrelated divergence (governing proto-block ordering vs
//! per-level stored order) remains out of this box's scope; see the ticket
//! for its repro (`t/defer-multi-cross-level-proto-block.t`).

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

/// Which visibility tier [`Interpreter::resolve_sequence`] should collect.
/// Added for ADR-0019 Phase E box E7 step 3 (private-as-sequence-query,
/// `todo/deep/adr0019-e5-e7-entry-routing.md` "E7 step 3") so the same
/// sequence builder can answer a private-method shadow probe without
/// disturbing any existing `Public` caller.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum MethodVisibility {
    /// The E4a/E6/E7-step-1/E7-step-2 behavior, unchanged: skip `is_private`
    /// defs and skip an ancestor-level submethod (`is_my && is_ancestor`).
    /// Also the only tier that consults `NativeCallBinding`/`Native`
    /// candidates — a private name (post `!`-stripping) can coincidentally
    /// collide with a public builtin/native row name, and neither native
    /// candidate kind is ever reachable through `self!name` dispatch, so
    /// `Private` skips both blocks entirely rather than surface a false hit.
    Public,
    /// Every `is_private` def at every chain level, with no `is_my`
    /// exclusion — mirrors `resolve_private_method_with_owner`/
    /// `resolve_private_method_any_owner` (`resolution_private_method.rs`),
    /// neither of which checks `is_my` at all.
    Private,
}

/// Whether [`Interpreter::resolve_sequence`]'s per-level lookup widens to
/// `Registry::get_method_overloads_with_role_fallback` (a role owner that was
/// never `.new`-punned still contributes its own candidates) or stays with
/// the plain `Registry::user_method_overloads` (a role owner with no pun
/// contributes nothing). Added for `todo/tickets/
/// e8a-deferral-shadow-sequence-is-role-blind.md`: `resolve_sequence` is also
/// the candidate source for live winner selection
/// ([`Interpreter::resolve_via_sequence_cache`]), and ADR-0019 F4a's rule is
/// that winner selection must never consult the role fallback — so this is
/// an explicit per-call choice, not a global switch. Only
/// [`Interpreter::shadow_check_deferral_sequence`]'s own `resolve_sequence`
/// call passes [`RoleFallback::Enabled`]; every other caller (including
/// [`Interpreter::resolve_via_sequence_cache`], whose `resolved_seq_cache`
/// key must keep meaning "no fallback") passes [`RoleFallback::Disabled`],
/// the pre-existing role-blind behavior, unchanged.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum RoleFallback {
    /// `Registry::user_method_overloads` only — the original, role-blind
    /// lookup every caller used before this ticket.
    Disabled,
    /// `Registry::get_method_overloads_with_role_fallback` — also surfaces a
    /// never-punned role's own candidates at the MRO level it occupies.
    Enabled,
}

/// One candidate in a [`ResolvedSequence`]. E4a only ever constructed `User`;
/// E4b adds the NativeCall-binding and native-row-catalog kinds (design
/// decision 4).
#[derive(Clone)]
pub(crate) enum ResolvedCandidate {
    /// A user-declared method, at its MRO level, in the class's stored
    /// declaration order. `level`/`stored_idx` are ADR-0019 E8a's structural
    /// additions (design decision 1 in
    /// `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`): `level` is
    /// this candidate's position in the `chain` it was built from (0 =
    /// receiver's own class), `stored_idx` its position within that level's
    /// `user_method_overloads` (declaration order). Together they reproduce
    /// today's observable deferral order (MRO level, then stored index,
    /// filtered per-call by signature match) without re-deriving it from a
    /// second walk — see [`Interpreter::shadow_check_deferral_sequence`].
    User {
        owner: TypeId,
        def: Arc<MethodDef>,
        // Not read by any production code path yet: E8a's own shadow probe
        // ([`Interpreter::shadow_check_deferral_sequence`]) relies on the
        // candidate Vec's own construction (insertion) order already being
        // `(level, stored_idx)`-ascending rather than re-deriving it from
        // these fields, and no real dispatch path consumes a sequence at
        // all yet (E9 is the box that builds a `DispatchCursor` over one).
        // Kept as struct fields now (not deferred to E9) because they are
        // structural facts of *this* box's own model — see the `User`
        // variant doc above — exercised directly by unit tests
        // (`#[cfg(test)]`, which `cargo build`/`clippy`'s dead-code pass
        // does not see). Mirrors [`ResolvedSequence::generation`]'s same
        // "measurement/future aid, not consulted yet" `#[allow(dead_code)]`.
        #[allow(dead_code)]
        level: u16,
        #[allow(dead_code)]
        stored_idx: u16,
    },
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
pub(crate) fn value_is_definite(value: &Value) -> bool {
    match value.view() {
        ValueView::Nil | ValueView::Package(_) | ValueView::CustomType(..) => false,
        ValueView::Slip(items) if items.is_empty() => false,
        _ => true,
    }
}

/// ADR-0019 E3 (design decision 5, `todo/deep/adr0019-e2-e4-resolver-core.md`):
/// the call-shape component of `resolved_seq_cache`'s key
/// `(TypeId, Symbol, CallShape)`. A [`ResolvedSequence`] only depends on the
/// receiver's owner chain and the call's arity/named-ness (via
/// [`NativeCallShape`]'s effect on which `Native` row, if any, is servable) —
/// not on the concrete argument values — so this is a small, `Copy` hash key
/// bucketing arity into `0 | 1 | 2 | 3+` and tracking whether any argument is
/// a named pair.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct CallShape {
    arity_bucket: u8,
    has_named: bool,
}

impl CallShape {
    pub(crate) fn for_args(args: &[Value]) -> CallShape {
        let has_named = args
            .iter()
            .any(|a| matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)));
        CallShape {
            arity_bucket: args.len().min(3) as u8,
            has_named,
        }
    }

    fn arg_count_for_native_shape(self) -> usize {
        self.arity_bucket as usize
    }
}

impl Interpreter {
    /// ADR-0019 E3 (design decision 5, `todo/deep/adr0019-e2-e4-resolver-core.md`):
    /// resolve `(cn, method)` for `args`/`target` via the cached
    /// [`ResolvedSequence`], replacing the live per-call MRO walk
    /// (`resolve_method_with_owner_impl`, reached via
    /// `resolve_method_with_owner_invocant`) `resolve_method_cached` used to
    /// perform at both of its cache-miss paths. The winner-selection algorithm
    /// itself, [`Self::pick_method_winner_from_sequence`], was verified
    /// (E3 slice 1) to reproduce `resolve_method_with_owner_impl` exactly —
    /// zero shadow-check mismatches across the full `t/` suite and the
    /// dispatch-heaviest roast directories — so this is authoritative, not a
    /// shadow probe.
    ///
    /// The sequence itself is cached per `(receiver TypeId, method, call
    /// shape)`: unlike `multi_resolve_cache` (which caches a resolved winner
    /// and therefore must never cache an ambiguous outcome), caching the
    /// candidate *universe* is safe regardless of ambiguity — ranking against
    /// the call's actual args happens fresh on every call from the cached
    /// candidates, exactly as it would from a freshly-walked one.
    pub(crate) fn resolve_via_sequence_cache(
        &mut self,
        cn: &str,
        method_sym: Symbol,
        args: &[Value],
        target: &Value,
    ) -> Option<(Symbol, MethodDef)> {
        // `resolve_method_with_owner_impl` resets this at the top of every
        // call (`resolution_method.rs:164`) — `pick_method_winner` only ever
        // sets it `true` on ambiguity, never clears it, so callers that check
        // it right after resolving (multi-resolve-cache's "never cache an
        // ambiguous outcome" rule) need it reset here too.
        self.dispatch_ambiguous = false;
        let owner = TypeId::intern(cn);
        let shape = CallShape::for_args(args);
        let mro_arc = self.class_mro(cn);
        let mro: Vec<Symbol> = mro_arc.iter().copied().collect();
        let seq = match self.resolved_seq_cache.get(&(owner, method_sym, shape)) {
            Some(cached) => cached.clone(),
            None => {
                let chain: Vec<TypeId> = mro.iter().map(|s| TypeId::from_symbol(*s)).collect();
                let native_shape = NativeCallShape::new(
                    shape.arg_count_for_native_shape(),
                    value_is_definite(target),
                );
                let built = Arc::new(self.resolve_sequence(
                    &chain,
                    method_sym,
                    native_shape,
                    MethodVisibility::Public,
                    RoleFallback::Disabled,
                ));
                self.resolved_seq_cache
                    .insert((owner, method_sym, shape), built.clone());
                built
            }
        };
        let role_bindings = self.registry().get_role_param_bindings(cn);
        self.pick_method_winner_from_sequence(
            &mro,
            cn,
            &seq.candidates,
            args,
            Some(target),
            role_bindings.as_ref(),
        )
    }

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
        visibility: MethodVisibility,
        role_fallback: RoleFallback,
    ) -> ResolvedSequence {
        let generation = self.registry().method_generation;
        let mut candidates = Vec::new();
        let mut native_binding_found = false;
        let mut native_row_found = false;
        for (level, owner) in chain.iter().enumerate() {
            let is_ancestor = level > 0;
            let owner_str = owner.as_str();
            let overloads = match role_fallback {
                RoleFallback::Disabled => self
                    .registry()
                    .user_method_overloads(owner_str, name.as_str()),
                RoleFallback::Enabled => self
                    .registry()
                    .get_method_overloads_with_role_fallback(owner_str, name.as_str()),
            };
            if let Some(overloads) = overloads {
                for (stored_idx, def) in overloads.into_iter().enumerate() {
                    let visible = match visibility {
                        MethodVisibility::Public => !(def.is_private || (def.is_my && is_ancestor)),
                        MethodVisibility::Private => def.is_private,
                    };
                    if !visible {
                        continue;
                    }
                    candidates.push(ResolvedCandidate::User {
                        owner: *owner,
                        def: Arc::new(def),
                        level: level as u16,
                        stored_idx: stored_idx as u16,
                    });
                }
            }
            if visibility == MethodVisibility::Private {
                // Private dispatch (`self!name`) never reaches a NativeCall
                // binding or a native-row catalog entry — both are indexed by
                // public builtin/binding names, and a coincidental name match
                // (e.g. a user `method !chars` vs. the `Str.chars` row) must
                // not surface as a false candidate here.
                continue;
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
        Self::drop_flattened_role_duplicate_candidates(&mut candidates);
        ResolvedSequence {
            generation,
            candidates,
        }
    }

    /// ADR-0019 E8a: the sequence-builder twin of
    /// `resolve_all_methods_with_owner`'s post-match
    /// `drop_flattened_role_duplicates` (`resolution_method.rs`), applied here
    /// at sequence *build* time instead — before any per-call argument
    /// matching, per design decision 1 ("submethod visibility and
    /// `drop_flattened_role_duplicates` are applied at sequence build time").
    /// Moving the dedup earlier is behavior-preserving: it removes candidates
    /// purely by owner identity (a role's own raw copy once a class level
    /// already carries the role-flattened copy), which does not interact with
    /// per-call argument matching -- the flattened copy has the same
    /// signature as the raw one it replaces, so filtering by owner before or
    /// after a `method_args_match_for_invocant` pass yields the same final
    /// matched set. See `resolution_method.rs`'s `drop_flattened_role_duplicates`
    /// doc comment for why the duplicate exists at all (mutsu keeps a composed
    /// role in the class's MRO; rakudo does not).
    ///
    /// **Self-owned pun exclusion (ADR-0019 F6 mut-dispatch family, found by its
    /// shadow-check sweep, `t/role-bless-pun.t`):** a role pun (`Service.bless`/
    /// `.new` on a bare role) copies the role's own methods into a synthetic
    /// class registered under the role's OWN name (`ensure_role_punned_to_class`),
    /// tagging each copy `role_origin = Some(role_name)` — the same name as the
    /// copy's own `owner`. That single MRO level would otherwise land in
    /// `flattened` from its own candidate and immediately delete itself, since
    /// the check below only compares owner identity, not whether some OTHER,
    /// differently-owned level actually carries the flattened copy this dedup
    /// exists to remove. Only a role_origin that names a DIFFERENT owner than
    /// the candidate carrying it indicates a genuine flattened-elsewhere
    /// duplicate (`class Foo does R`: the "Foo"-owned flattened copy has
    /// `role_origin = Some("R")`, owner != role_origin, and it is that fact
    /// which makes the separate raw "R"-owned MRO level redundant) — matching
    /// `resolve_method_with_owner_impl`, which this sequence is meant to
    /// reproduce and which never drops a pun's own single-level candidate.
    fn drop_flattened_role_duplicate_candidates(candidates: &mut Vec<ResolvedCandidate>) {
        let flattened: HashSet<String> = candidates
            .iter()
            .filter_map(|c| match c {
                ResolvedCandidate::User { owner, def, .. } => def
                    .role_origin
                    .as_deref()
                    .filter(|ro| *ro != owner.as_str())
                    .map(str::to_string),
                ResolvedCandidate::NativeCallBinding { .. } | ResolvedCandidate::Native { .. } => {
                    None
                }
            })
            .collect();
        if flattened.is_empty() {
            return;
        }
        candidates.retain(|c| match c {
            ResolvedCandidate::User { owner, .. } => !flattened.contains(owner.as_str()),
            ResolvedCandidate::NativeCallBinding { .. } | ResolvedCandidate::Native { .. } => true,
        });
    }

    /// ADR-0019 E3: reproduce `resolve_method_with_owner_impl`'s exact
    /// non-multi/multi decision algorithm — see the module doc — over a
    /// [`ResolvedSequence`]'s flat, `(level, stored_idx)`-ordered `User`
    /// candidates, instead of a live per-call MRO walk. `Native`/
    /// `NativeCallBinding` candidates never participate in this ranking (the
    /// same rule [`Self::match_sequence_candidates`] already applies).
    ///
    /// Same `where`-clause care point as the deferral-sequence probe: a
    /// `where` clause is user code whose dynamic-variable writes are an
    /// observable side effect of `method_args_match_for_invocant` — the
    /// caller is responsible for skipping this call entirely when any
    /// candidate carries one (both current callers already do, via their own
    /// `has_where_candidate` guard, so this function does not re-check it).
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn pick_method_winner_from_sequence(
        &mut self,
        mro: &[Symbol],
        class_name: &str,
        candidates: &[ResolvedCandidate],
        arg_values: &[Value],
        invocant: Option<&Value>,
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
    ) -> Option<(Symbol, MethodDef)> {
        let mut all_matches: Vec<(Symbol, MethodDef)> = Vec::new();
        let mut idx = 0;
        while idx < candidates.len() {
            let ResolvedCandidate::User { level, .. } = &candidates[idx] else {
                idx += 1;
                continue;
            };
            let this_level = *level;
            let start = idx;
            while idx < candidates.len()
                && matches!(&candidates[idx], ResolvedCandidate::User { level, .. } if *level == this_level)
            {
                idx += 1;
            }
            let group = &candidates[start..idx];
            let any_multi = group
                .iter()
                .any(|c| matches!(c, ResolvedCandidate::User { def, .. } if def.is_multi));
            if !any_multi && all_matches.is_empty() {
                let mut first_visible: Option<(Symbol, MethodDef)> = None;
                for c in group {
                    let ResolvedCandidate::User { owner, def, .. } = c else {
                        unreachable!("group only contains User candidates")
                    };
                    if first_visible.is_none() {
                        first_visible = Some((owner.symbol(), (**def).clone()));
                    }
                    if self.method_args_match_for_invocant(
                        class_name,
                        def,
                        arg_values,
                        role_bindings,
                        invocant,
                    ) {
                        return Some((owner.symbol(), (**def).clone()));
                    }
                }
                return first_visible;
            }
            for c in group {
                let ResolvedCandidate::User { owner, def, .. } = c else {
                    unreachable!("group only contains User candidates")
                };
                if self.method_args_match_for_invocant(
                    class_name,
                    def,
                    arg_values,
                    role_bindings,
                    invocant,
                ) {
                    all_matches.push((owner.symbol(), (**def).clone()));
                }
            }
        }
        if all_matches.is_empty() {
            None
        } else {
            self.pick_method_winner(mro, arg_values, invocant, all_matches)
        }
    }

    /// ADR-0019 E8a: "ranker extracted to consume a candidate slice" — the
    /// per-call signature-match filtering step both
    /// [`Self::shadow_check_resolver_chain`] (E4a's winner probe) and
    /// [`Self::shadow_check_deferral_sequence`] (this box's deferral-list
    /// probe) need before ranking or fingerprinting a sequence's `User`
    /// candidates. Extracted so the two probes share one filtering pass
    /// instead of each carrying its own copy of the loop. Skips every
    /// non-`User` candidate (`Native`/`NativeCallBinding` never enter the
    /// method ranking ladder). Returns matches in the sequence's own order
    /// (`(level, stored_idx)` construction order), unchanged by this
    /// extraction — ranking itself stays [`Self::pick_method_winner`],
    /// called separately by the winner probe; this only produces its input.
    pub(crate) fn match_sequence_candidates(
        &mut self,
        class_name: &str,
        candidates: &[ResolvedCandidate],
        arg_values: &[Value],
        invocant: Option<&Value>,
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
    ) -> Vec<(Symbol, MethodDef)> {
        let mut matched: Vec<(Symbol, MethodDef)> = Vec::new();
        for c in candidates {
            let ResolvedCandidate::User { owner, def, .. } = c else {
                continue;
            };
            if self.method_args_match_for_invocant(
                class_name,
                def,
                arg_values,
                role_bindings,
                invocant,
            ) {
                matched.push((owner.symbol(), (**def).clone()));
            }
        }
        matched
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
        let chain = self.dispatch_mro(invocant);
        self.shadow_check_resolver_chain(
            site,
            class_name,
            method,
            method_sym,
            arg_values,
            Some(invocant),
            &chain,
            MethodVisibility::Public,
            real,
        );
    }

    /// The `chain`/`invocant`-parametrized core of [`Self::shadow_check_resolver`]
    /// (ADR-0019 Phase E box E7, second consumer family — qualified dispatch,
    /// `todo/deep/adr0019-e5-e7-entry-routing.md` "E7 step 2"). Extracted so a
    /// caller with no receiver *value* of the resolution target's type — e.g.
    /// `self.Owner::method(...)`, where the chain must be rooted at the
    /// qualifier class NAME rather than derived from an instance — can still
    /// reuse the exact same shadow-ranking logic by passing a chain it built
    /// itself (typically via `self.dispatch_mro(&Value::package(...))`) and
    /// `invocant: None`. [`Self::shadow_check_resolver`] remains a thin
    /// wrapper over this for its own receiver-chain callers, so their
    /// behavior is unchanged by this split.
    ///
    /// A `None` invocant is treated as "not DEFINITE" for the native-row
    /// shape (mirroring [`value_is_definite`]'s treatment of a bare type
    /// object/`Package`) and skips the invocant type-constraint check inside
    /// `method_args_match_for_invocant` entirely (that function already
    /// handles `Option<&Value>` this way) — both match how
    /// `resolve_method_with_owner`'s registry-only walk itself calls
    /// `resolve_method_with_owner_impl(..., invocant: None)` for the case
    /// this is shadowing.
    ///
    /// `visibility` (added for ADR-0019 Phase E box E7 step 3,
    /// private-as-sequence-query) selects which
    /// [`MethodVisibility`] tier `resolve_sequence` collects — every existing
    /// caller (both `Public`-chain callers above, plus qualified dispatch's
    /// `dispatch_qualified_instance_method`) passes `MethodVisibility::Public`
    /// unchanged; only the new private-dispatch shadow probe passes `Private`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn shadow_check_resolver_chain(
        &mut self,
        site: &'static str,
        class_name: &str,
        method: &str,
        method_sym: Symbol,
        arg_values: &[Value],
        invocant: Option<&Value>,
        chain: &[TypeId],
        visibility: MethodVisibility,
        real: Option<&(Symbol, MethodDef)>,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let saved_ambiguous = self.dispatch_ambiguous;
        let definite = invocant.map(value_is_definite).unwrap_or(false);
        let native_shape = NativeCallShape::new(arg_values.len(), definite);
        let seq = self.resolve_sequence(
            chain,
            method_sym,
            native_shape,
            visibility,
            RoleFallback::Disabled,
        );
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
        let shadow = self.pick_method_winner_from_sequence(
            &mro,
            class_name,
            &seq.candidates,
            arg_values,
            invocant,
            role_bindings.as_ref(),
        );
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
        let seq = self.resolve_sequence(
            &chain,
            method_sym,
            native_shape,
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
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

    /// ADR-0019 E8a shadow probe (`MUTSU_VM_STATS`-gated, a no-op otherwise):
    /// does the sequence's own `(level, stored_idx)` construction order,
    /// filtered per-call by [`Self::method_args_match_for_invocant`] and with
    /// the chosen winner's fingerprint removed, reproduce the "remaining"
    /// deferral list [`Interpreter::push_method_dispatch_frame`] builds today
    /// via a second, unranked `resolve_all_methods_with_owner` walk plus
    /// fingerprint-based winner removal? Per design decision 1 in
    /// `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`, that
    /// walk's own output IS the target this box's `level`/`stored_idx`
    /// fields are meant to reproduce — deferral order = sequence order (MRO
    /// level, then stored index) filtered by per-call signature match.
    /// Comparison is **order-sensitive** (a `Vec<u64>` equality, not a set):
    /// deferral order is user-observable through repeated `nextsame`/
    /// `callsame`.
    ///
    /// `chosen_fp` is the caller's already-computed fingerprint of the
    /// dispatch winner (`None` when no candidate was chosen at all); passing
    /// it in avoids re-deriving the winner here, which this probe does not
    /// need to do — [`Self::shadow_check_resolver`] already shadow-checks
    /// winner selection independently at the `resolve_method_cached`
    /// boundaries, and per design decision 1 the winner ranking itself is
    /// unchanged by `level`/`stored_idx` (they are deferral-order-only
    /// facts), so there is no new winner-selection logic here to verify.
    ///
    /// Same `where`-clause care point as
    /// [`Self::shadow_check_resolver_chain`]: a `where` clause is user code
    /// whose dynamic-variable writes are an observable side effect of the
    /// REAL match already performed by `push_method_dispatch_frame`'s own
    /// `resolve_all_methods_with_owner` call — running
    /// `method_args_match_for_invocant` a second time here for a
    /// `where`-carrying candidate would duplicate that side effect, so any
    /// candidate with a `where` clause anywhere in the sequence skips the
    /// whole probe.
    ///
    /// **Invocant-blind matching, deliberately**: `resolve_all_methods_with_owner`
    /// itself always calls `method_args_match_for_invocant(..., invocant:
    /// None)` (`resolution_method.rs`) — the deferral list is NOT filtered by
    /// the invocant's type/definedness (`:U:`/`:D:` smileys), only by the
    /// call's non-invocant argument shape. A first version of this probe
    /// passed `Some(invocant)` here, which is *stricter* than the real
    /// target and produced spurious mismatches on every `::?ROLE:U:`/
    /// `::?ROLE:D:` multi pair (`t/role-ud-multi-dispatch.t` et al.): the
    /// real "remaining" list still contains the sibling smiley candidate
    /// (raku's own `nextsame`/`callsame` walk does not re-check the
    /// invocant), so the shadow list must be built the same invocant-blind
    /// way to actually be a shadow of this target, not an improvement on it.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn shadow_check_deferral_sequence(
        &mut self,
        receiver_class: &str,
        method: &str,
        arg_values: &[Value],
        invocant: &Value,
        chosen_fp: Option<u64>,
        real_remaining: &[(Symbol, MethodDef)],
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let method_sym = Symbol::intern(method);
        let mro = self.class_mro(receiver_class);
        let chain: Vec<TypeId> = mro.iter().map(|s| TypeId::from_symbol(*s)).collect();
        let native_shape = NativeCallShape::new(arg_values.len(), value_is_definite(invocant));
        // RoleFallback::Enabled — the whole point of this ticket's fix: the
        // real deferral walker (`resolve_deferral_expansion`, E9a) consults
        // `Registry::get_method_overloads_with_role_fallback`, so the shadow
        // list must widen the same way or every role-in-MRO shape reports a
        // false mismatch. This is the ONLY `resolve_sequence` call site that
        // passes `Enabled` — see the `RoleFallback` doc comment.
        let seq = self.resolve_sequence(
            &chain,
            method_sym,
            native_shape,
            MethodVisibility::Public,
            RoleFallback::Enabled,
        );
        let has_where_candidate = seq.candidates.iter().any(|c| {
            let ResolvedCandidate::User { def, .. } = c else {
                return false;
            };
            def.param_defs.iter().any(|p| p.where_constraint.is_some())
        });
        if has_where_candidate {
            return;
        }
        let role_bindings = self.registry().get_role_param_bindings(receiver_class);
        let matched = self.match_sequence_candidates(
            receiver_class,
            &seq.candidates,
            arg_values,
            None,
            role_bindings.as_ref(),
        );
        // Mirrors `push_method_dispatch_frame`'s own loop shape exactly (fp
        // compare against `chosen_fp` first, THEN the hidden-defer-parent
        // filter `should_skip_defer_method_candidate`) so a candidate that
        // happens to be both the winner AND nominally hidden is still
        // dropped for the winner reason, not double-counted as a hidden-
        // parent divergence.
        let mut shadow_fps: Vec<u64> = Vec::new();
        let mut skipped_chosen = false;
        for (owner, def) in &matched {
            let fp = self.method_def_fingerprint(def);
            if !skipped_chosen && Some(fp) == chosen_fp {
                skipped_chosen = true;
                continue;
            }
            if self.should_skip_defer_method_candidate(receiver_class, owner.as_str()) {
                continue;
            }
            shadow_fps.push(fp);
        }
        let real_fps: Vec<u64> = real_remaining
            .iter()
            .map(|(_, def)| self.method_def_fingerprint(def))
            .collect();
        let matched = shadow_fps == real_fps;
        crate::vm::vm_stats::record_deferral_shadow_check(matched, || {
            format!(
                "class={receiver_class} method={method} real_len={} shadow_len={}",
                real_fps.len(),
                shadow_fps.len()
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
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("greet"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
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
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("only-base"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
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
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("only-base"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        assert_eq!(seq.candidates.len(), 1);
    }

    #[test]
    fn resolve_sequence_is_empty_for_an_unknown_method() {
        let mut i = interp();
        i.run("class Base { }").unwrap();
        let chain = vec![TypeId::intern("Base")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("nope"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
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
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("tap"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
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
            MethodVisibility::Public,
            RoleFallback::Disabled,
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
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        assert!(
            !seq.candidates
                .iter()
                .any(|c| matches!(c, ResolvedCandidate::Native { .. })),
            "Str.chars is an A0 row and must not surface for a 1-arg call"
        );
    }

    /// ADR-0019 E7 step 3: `MethodVisibility::Private` finds a private
    /// method, and (unlike `Public`) does not exclude it from an ancestor
    /// level — neither `resolve_private_method_with_owner` nor
    /// `resolve_private_method_any_owner` checks `is_my` at all.
    #[test]
    fn resolve_sequence_private_finds_a_private_method() {
        let mut i = interp();
        i.run("class Base { method !secret { 'shh' } }").unwrap();
        let chain = vec![TypeId::intern("Base")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("secret"),
            default_shape(),
            MethodVisibility::Private,
            RoleFallback::Disabled,
        );
        let owners: Vec<&str> =
            seq.candidates
                .iter()
                .filter_map(|c| match c {
                    ResolvedCandidate::User { owner, .. } => Some(owner.as_str()),
                    ResolvedCandidate::NativeCallBinding { .. }
                    | ResolvedCandidate::Native { .. } => None,
                })
                .collect();
        assert_eq!(owners, vec!["Base"]);
    }

    /// The `Public` tier's existing behavior is unchanged by adding
    /// `Private`: a private method must never appear in a `Public` sequence.
    #[test]
    fn resolve_sequence_public_excludes_a_private_method() {
        let mut i = interp();
        i.run("class Base { method !secret { 'shh' } }").unwrap();
        let chain = vec![TypeId::intern("Base")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("secret"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        assert!(
            seq.candidates.is_empty(),
            "a private method must not appear in a Public sequence"
        );
    }

    /// `Private` never surfaces a `NativeCallBinding`/`Native` candidate,
    /// even when the (post-`!`-stripping) name coincides with a public
    /// builtin/native row name — private dispatch can never reach either.
    #[test]
    fn resolve_sequence_private_never_surfaces_a_native_candidate() {
        let mut i = interp();
        let chain = vec![TypeId::intern("Str")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("chars"),
            NativeCallShape::new(0, true),
            MethodVisibility::Private,
            RoleFallback::Disabled,
        );
        assert!(
            seq.candidates.is_empty(),
            "Private must not surface Str.chars's public Native row"
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
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("chomp"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        assert!(
            seq.candidates.is_empty(),
            "hardcoded native-method names must not apply to an ancestor level"
        );
    }

    /// ADR-0019 E8a: `level` is the candidate's position in the chain (0 =
    /// receiver's own class); `stored_idx` is its position within that
    /// level's own declaration order. Two `multi method` overloads on the
    /// receiver's class come before the single ancestor override.
    #[test]
    fn resolve_sequence_assigns_level_and_stored_idx() {
        let mut i = interp();
        i.run(
            "class Base { method greet { 'base' } }\n\
             class Child is Base {\n\
               multi method greet(Int $x) { 'child-int' }\n\
               multi method greet(Str $x) { 'child-str' }\n\
             }",
        )
        .unwrap();
        let chain = vec![TypeId::intern("Child"), TypeId::intern("Base")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("greet"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        let facts: Vec<(&str, u16, u16)> =
            seq.candidates
                .iter()
                .filter_map(|c| match c {
                    ResolvedCandidate::User {
                        owner,
                        level,
                        stored_idx,
                        ..
                    } => Some((owner.as_str(), *level, *stored_idx)),
                    ResolvedCandidate::NativeCallBinding { .. }
                    | ResolvedCandidate::Native { .. } => None,
                })
                .collect();
        assert_eq!(
            facts,
            vec![("Child", 0, 0), ("Child", 0, 1), ("Base", 1, 0)],
            "expected declaration-order stored_idx within Child's level 0, then Base at level 1"
        );
    }

    /// ADR-0019 E8a: `drop_flattened_role_duplicate_candidates` (applied
    /// inside `resolve_sequence` at build time) removes a composed role's own
    /// raw MRO entry once a class level already carries the flattened copy —
    /// the sequence-builder twin of `resolution_method.rs`'s
    /// `drop_flattened_role_duplicates`, moved to build time per design
    /// decision 1.
    #[test]
    fn resolve_sequence_drops_a_flattened_role_duplicate_at_build_time() {
        let mut i = interp();
        i.run("role R { method greet { 'r' } }\nclass C does R { }")
            .unwrap();
        let mro = i.class_mro("C");
        let chain: Vec<TypeId> = mro.iter().map(|s| TypeId::from_symbol(*s)).collect();
        assert!(
            chain.iter().any(|t| t.as_str() == "R"),
            "a composed role stays in mutsu's own MRO (see the module-level doc on the dedup)"
        );
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("greet"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        let owners: Vec<&str> =
            seq.candidates
                .iter()
                .filter_map(|c| match c {
                    ResolvedCandidate::User { owner, .. } => Some(owner.as_str()),
                    ResolvedCandidate::NativeCallBinding { .. }
                    | ResolvedCandidate::Native { .. } => None,
                })
                .collect();
        assert_eq!(
            owners,
            vec!["C"],
            "only the class-level flattened copy should remain; the role's own raw copy is dropped"
        );
    }

    /// ADR-0019 E3: a non-multi override on a more-derived class must win
    /// even when its own signature does not match the call — the ancestor's
    /// (matching) candidate must NOT be reached. This is the exact rule the
    /// original E4a shadow probe did not model (see the module doc).
    #[test]
    fn pick_method_winner_from_sequence_non_multi_override_wins_without_matching() {
        let mut i = interp();
        i.run(
            "class Base { method greet($x) { 'base' } }\n\
             class Child is Base { method greet() { 'child' } }",
        )
        .unwrap();
        let real = i
            .resolve_method_with_owner("Child", "greet", &[Value::int(1)])
            .expect("real resolver should still return the sole non-matching candidate");
        assert_eq!(real.0.as_str(), "Child");
        let chain = vec![TypeId::intern("Child"), TypeId::intern("Base")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("greet"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        let mro: Vec<Symbol> = chain.iter().map(|t| t.symbol()).collect();
        let shadow = i.pick_method_winner_from_sequence(
            &mro,
            "Child",
            &seq.candidates,
            &[Value::int(1)],
            None,
            None,
        );
        let shadow = shadow.expect("shadow winner must also return Child.greet");
        assert_eq!(shadow.0.as_str(), "Child");
    }

    /// ADR-0019 E3: multi candidates across two MRO levels still rank by the
    /// existing `pick_method_winner` tie-break ladder (type-distance here).
    #[test]
    fn pick_method_winner_from_sequence_ranks_multi_across_levels() {
        let mut i = interp();
        i.run(
            "class Base { multi method greet(Any $x) { 'base-any' } }\n\
             class Child is Base { multi method greet(Int $x) { 'child-int' } }",
        )
        .unwrap();
        let chain = vec![TypeId::intern("Child"), TypeId::intern("Base")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("greet"),
            default_shape(),
            MethodVisibility::Public,
            RoleFallback::Disabled,
        );
        let mro: Vec<Symbol> = chain.iter().map(|t| t.symbol()).collect();
        let shadow = i
            .pick_method_winner_from_sequence(
                &mro,
                "Child",
                &seq.candidates,
                &[Value::int(1)],
                None,
                None,
            )
            .expect("Int arg should match both candidates");
        assert_eq!(
            shadow.0.as_str(),
            "Child",
            "the more specific Int candidate must win over the Any one"
        );
    }

    fn default_shape() -> NativeCallShape {
        NativeCallShape::new(0, true)
    }
}

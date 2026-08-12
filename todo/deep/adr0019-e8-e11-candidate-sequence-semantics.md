# ADR-0019 E8/E9/E10/E11 design: multi ordering, deferral cursors, wrap generations, arity retirement

Design pass for Phase E boxes E8 (multi/proto/submethod ordering in the candidate sequence),
E9 (resolver cursors for `samewith`/`nextsame`/`callsame`/`nextwith`), E10 (wrap/unwrap into
canonical entries + generation), and E11 (retire arity-specific lookup entry points). Depends
on the E4 `ResolvedSequence` (`adr0019-e2-e4-resolver-core.md`) and the E5-E7 routing
(`adr0019-e5-e7-entry-routing.md`). E9 is the highest-semantic-risk box of the whole phase and
carries a mandatory raku verification campaign (D8-2/D3-8 precedent — 拙速厳禁). No code has
landed for these boxes yet.

## Facts the design rests on (survey results, 2026-08-09)

**Multi/submethod/proto (E8):**

- Method multis and sub multis rank with **two hand-synchronized ladders**: methods via
  `method_candidate_type_distance` + the `narrowness` tuple + MRO-index + `is default`
  (`resolution_method.rs:116-382`; the sync comment is at :248-253), subs via
  `candidate_specificity_rank`/`candidate_type_distance` (`dispatch_candidates.rs`). Method
  multis tie-break by **stored order** (`narrowed.first()`, :296-298); subs have an explicit
  `decl_order` sort key (`dispatch_resolve.rs:79-90`).
- The submethod no-inherit rule is copy-pasted at six sites plus WALK's inverted variant
  (enumerated in the E2-E4 doc); BUILD/TWEAK have their own precomputed plan
  (`ctor_phase_plan.rs:31-167`) whose pinning bypasses full dispatch only when
  `native_call_specs.is_empty() && !has_any_wrap_chains()` (:231-234).
- Proto methods live in a **third table**, `Registry::proto_methods:
  HashMap<(String, String), FunctionDef>` (`registry.rs:267-271`), with their own MRO walk
  (`dispatch_proto.rs:222-241`). A proto's `{*}` does not hand candidates to the callee — it
  sets a one-shot `proto_method_skip` flag and **re-enters `call_method_with_values` by name**
  (`dispatch_proto_call.rs:14-57`). The VM `{*}` arm falls back to the interpreter for the
  method case (`vm_call_func_ops.rs:1704-1710`).
- Role composition appends candidates per role in iteration order
  (`registration_class_compose.rs:352-356`); `resolve_class_stub_requirements` then *mutates*
  candidate order (class wins, role duplicates removed — `registration.rs:347-369`);
  `drop_flattened_role_duplicates` (`resolution_method.rs:613-630`) de-dups again at
  resolution time because the composed role is also an MRO entry.
- `.^lookup(...).candidates` uses yet another ordering (reversed MRO, multi owners only,
  `methods_classhow_lookup.rs:238-292`), and **wrap identity is keyed to that stored candidate
  index** (`__mutsu_lookup_candidate_idx`, :279-281).

**Deferral (E9):** three separate stacks, none a cursor —

- `method_dispatch_stack: Vec<MethodDispatchFrame>` (`mod.rs:1797`, struct at
  `decl_types.rs:165-176`): `remaining` is recomputed by `resolve_all_methods_with_owner` (a
  second, unranked walk) with the winner removed by **AST body fingerprint** comparison; built
  at three push sites (`class_dispatch.rs:218-240/277-283/358-364`,
  `accessors_state.rs:748-836`, `methods_mixin_dispatch.rs:200-225`).
- `wrap_dispatch_stack: Vec<WrapDispatchFrame>` (`mod.rs:1831`): `remaining` is a
  `Vec<Value>` of wrapper code objects; a method wrap is marked `sub_id == 0` and falls
  through to the method stack when exhausted (`builtins_dispatch_next.rs:365-427`); the
  original-method leg re-enters **by name** (:391) guarded by `wrap_skip_once`.
- `multi_dispatch_stack` (subs) plus `samewith_context_stack` — `samewith` re-enters
  `call_method_with_values`/`call_function` **by name** (`builtins_dispatch_next.rs:101-118`).
- When the user MRO is exhausted, four synthesized native fallbacks are force-pushed with
  empty `remaining`: `native_{grammar_parse,mu_base,array_storage,metamodel}_next_candidate`
  (`builtins_dispatch_next.rs:181-310`).
- `dispatch_next_candidate` searches the stacks in fixed priority wrap → method → multi →
  native-metamodel → `Mu.new` fallback (:358-903); `lastcall` truncates the topmost frame
  (:62-75); `nextcallee` mirrors the search (:938-983).

**Wrap (E10):** all state on `Interpreter`, not the registry — `wrap_chains` (by sub id),
`method_wrap_chains` (by `(class, method, candidate_idx)`), five sibling maps
(`mod.rs:1819-1850`). Invalidation is `fn_resolve_gen += 1` at four sites
(`methods_sub.rs:883/927/951`, `methods_call_dispatch.rs:2240`) — **never**
`method_generation` — and two wrap paths invalidate **nothing** (`.wrap` on a `^lookup`
candidate, `methods_sub.rs:796-799`; `.wrap` on a `Method` object,
`methods_instance_ops.rs:2202-2205`). Dispatch stays correct only via the global prefilter
`has_any_wrap_chains()` (`accessors_state.rs:842-844`), which disables the fast method cache
for the whole program as soon as one wrap exists. `.unwrap`/`restore` never remove
`method_wrap_chains` entries (only class redeclaration purges them,
`registration_class_validate.rs:377`).

**Arity entries (E11):** `native_method_{0,1,2}arg` are called as *probes* (not just
handlers) at: the `.^can` live probe with a dummy `Value::NIL` arg
(`methods_classhow_method_obj.rs:386-388`), `accessors_stack.rs:189` (`.is_some()` existence
test), `builtins_collection.rs:384`, the arity-switch ladders
(`methods_call_dispatch.rs:2795-2800`, `vm_native_dispatch.rs:369-377`,
`methods_instance_ops.rs:1454-1470`), and the mut slow path's direct calls
(`methods_mut_dispatch.rs:1703-1921`). The `#[cfg(test)]`
`native_responds_to`/`native_method_arities` probes are already runtime-retired (B1/B2).

## Design decisions

**1. E8: the sequence encodes structure; ranking stays per-call; the two ladders are NOT
unified.** `ResolvedCandidate` gains the structural facts the ranker and the deferral order
need:

```rust
User { owner: TypeId, def: Arc<MethodDef>, level: u16, stored_idx: u16 }
```

- `level`/`stored_idx` reproduce today's observable orders: winner selection = existing method
  ladder over the sequence; deferral order = sequence order (MRO level, then stored index)
  filtered by per-call signature match — which is exactly what
  `resolve_all_methods_with_owner` computes today, so that walker is deleted once E9 consumes
  the sequence.
- Submethod visibility (`is_my && level > 0`) and `drop_flattened_role_duplicates` are applied
  at sequence *build* time (one site each). The composed-order mutations
  (`resolve_class_stub_requirements`) happen before sync, so the sequence inherits them — no
  new mechanism.
- The box text says "without changing tie-breaking or role conflicts": unifying the method and
  sub ranking ladders is explicitly **out of scope** (file the asymmetry — method multis lack
  `decl_order` — as a ticket with a raku repro if one exists; do not fix it silently inside
  E8).
- Proto methods: `Registry::proto_methods` folds into `MethodEntry` as a
  `proto: Option<ProtoMethodDef>` column; the sequence's `proto` slot (already in the E4
  schema) makes proto interception a resolver outcome instead of the standalone
  `lookup_proto_method` MRO walk. The `{*}` re-entry rewrite itself is E9 (it needs cursors).

**2. E9: one `DispatchCursor` replaces recomputation; wrappers become sequence prefix
entries.**

```rust
pub(crate) struct DispatchCursor {
    seq: Arc<ResolvedSequence>,   // includes Native candidates — the four synthesized
    next: usize,                  //   fallbacks become ordinary sequence tail entries
    invocant: Option<Value>,
    args: Vec<Value>,             // the original binding, for nextsame/callsame
    // rw bookkeeping carried over from MethodDispatchFrame.rw_params
}
```

- At dispatch time the chosen entry pushes a cursor positioned after the winner. `callsame`/
  `nextsame` advance from `next`, applying the per-call signature filter as they go (matching
  today's "remaining = signature-matching candidates in MRO order" semantics); `callwith`/
  `nextwith` do the same with replacement args; `lastcall` sets `next = seq.len()`;
  `nextcallee` peeks. `samewith` re-runs the *ranker* over the same sequence with new args (not
  a name re-entry), preserving its restart-from-the-top semantics.
- Wrap chains materialize as cursor-prefix entries: resolving a wrapped candidate yields
  `[Wrapper(outermost) … Wrapper(innermost), the candidate, …rest of sequence]`, so `callsame`
  inside a wrapper is ordinary cursor advancement and the `sub_id == 0` sentinel, the
  `wrap_skip_once` flag, and the by-name original-method re-entry all disappear. The mid-MRO
  wrap interception (`builtins_dispatch_next.rs:445-499`) likewise falls out: the next
  candidate's wrappers are already its prefix.
- The winner-removal-by-fingerprint hack disappears because the cursor knows the winner's
  index; the fingerprint comparison sites (`class_dispatch.rs:225-232`,
  `accessors_state.rs:808-818`) are deleted with their frames.
- The proto `{*}` becomes: run the ranker over `cursor.seq` excluding the proto itself —
  deleting `proto_method_skip` and the arg-source save/restore dance
  (`dispatch_proto_call.rs:45-55`).
- Migration is frame-by-frame: E9a converts `MethodDispatchFrame` to the cursor (method calls
  only, wrap prefix not yet), E9b folds `WrapDispatchFrame` in, E9c converts proto `{*}` and
  `samewith`. The sub-side `multi_dispatch_stack` is out of scope here (it is Phase C/F
  territory — sub multis already carry their candidate list; converting them to the same
  cursor type is an optional follow-up once methods prove the shape).

**3. E9 verification campaign (mandatory, before E9a lands).** A table of raku ground truths,
each becoming a `t/` pin, covering at least: nextsame through (a) multi candidates in one
class, (b) an inherited same-name method, (c) a role-composed method that also exists in the
class (flattened-duplicate case), (d) `is Array` subclass reaching native push (today's
`native_array_storage_next_candidate`), (e) Grammar `.parse` override calling nextsame; wrap
interleavings: callsame inside a wrapper of (f) a plain method, (g) a multi candidate, (h) a
method that also has an inherited version — including wrap applied mid-MRO; (i) `lastcall`
inside a wrapper; (j) samewith with changed args from a multi candidate; (k) callwith
arg-rebinding with `is rw` params; (l) `{*}` in a proto method with multi candidates across
MRO levels; (m) BUILD calling callsame. Divergences found between raku and current mutsu are
tickets first, design inputs second — do not encode a mutsu bug into the cursor semantics.

**4. E10: wraps move under the registry generation; the global prefilter dies.**

- `method_wrap_chains` moves into the registry keyed `(owner: Symbol, method: Symbol,
  candidate_idx: u16)` (keeping the stored-index identity that `.^lookup` wraps already use;
  candidate-index stability across composition is exactly as good as today — no better, no
  worse). Every wrap/unwrap/restore path — including the two currently-invalidation-free ones —
  goes through registry write entries that bump `method_generation` (the ADR's Phase B note
  already promises "no new invalidation hook needed — Phase B's scheme was built
  write-path-agnostic").
- Sub-side `wrap_chains` stays interpreter-owned (sub dispatch is not registry-keyed) but its
  mutation sites also bump `method_generation` so one generation covers all resolved-sequence
  invalidation; `fn_resolve_gen` bumps stay until F5 retires that scheme.
- After E3/E5 route reads through the generation-checked sequence cache,
  `has_any_wrap_chains()` and its five prefilter call sites are deleted — un-gating the fast
  cache for every program that uses `.wrap` anywhere (a measurable perf win to cite in the PR).
- `.unwrap`/`restore` gain the missing `method_wrap_chains` removal (pre-existing leak: a
  restored method wrap today survives as a stale chain entry, kept live only by prefilter
  overhead); raku-verify `.unwrap` semantics on method wraps first.

**5. E11: probes become row lookups; the arity functions become invocation-only.** After
E5-E7, the remaining *lookup* uses of `native_method_{0,1,2}arg` are replaced by E2 row
queries: the `.^can` dummy-NIL probe (also a correctness fix — dummy-arg probing
false-negatives on arms that type-check the argument), the `accessors_stack.rs:189` existence
test, `builtins_collection.rs:384`, and the three arity-switch ladders (which by then are the
invocation tier behind the resolver's Native decision). Completion criterion (grep-based, D10
style): **no caller of `native_method_{0,1,2}arg` outside (i) the single native-invocation
helper the resolver's Native arm calls, (ii) internal recursion within `builtins/`, and (iii)
`#[cfg(test)]`** — any new probe-shaped caller is a regression against this box.

## Slice plan

- **E8a** — sequence gains `level`/`stored_idx` + build-time submethod/duplicate rules; ranker
  extracted to consume a candidate slice; shadow-compare winner AND deferral list against
  `resolve_all_methods_with_owner` output under a counter (list equality by fingerprint).
- **E8b** — proto methods into `MethodEntry`; `lookup_proto_method` deleted;
  `try_proto_method_body` reads the sequence's proto slot.
- **E9-pre** — the raku verification campaign (docs + `t/` pins only; any mutsu divergence
  filed as tickets).
- **E9a/E9b/E9c** — cursor cutovers per decision 2's migration order, each with the campaign's
  relevant pins green and local `make roast`.
- **E10a** — registry-owned method wraps + generation bumps on all wrap mutations (+ the
  unwrap leak fix). **E10b** — delete `has_any_wrap_chains` prefilter once E3 is in (order
  with E3, whichever lands second does the deletion); bench evidence.
- **E11** — probe retirement + the grep criterion added to the architectural guard test (G2).

## Risk notes

E9 changes the observable order of user-visible dispatch chains — the one area where "CI
catches it" is least true because chain-order bugs hide in rarely-exercised deferral paths.
Hence the mandatory pre-campaign and per-slice `make roast`. E10's registry move touches the
thread-fork COW path (`runtime_thread.rs`) — wraps created in a child must not leak to the
parent (today's interpreter-owned maps get that isolation for free; the registry COW gives the
same, but add a test). E11 is mechanical once E5-E7 land; do not start it earlier.

## E8a: sequence structural fields land; deferral-list shadow check finds one real bug (fixed) and one pre-existing gap (documented, not fixed)

Landed 2026-08-12, exactly as scoped by the slice plan above. Full detail also in the ADR's E8
progress note (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`); this
section is the design doc's own record of the same slice.

**Structural fields.** `ResolvedCandidate::User` (`resolution_sequence.rs`) gained `level: u16`
(position in the chain, 0 = receiver's own class) and `stored_idx: u16` (position within that
level's `user_method_overloads`, i.e. declaration order), set in `resolve_sequence`'s existing
per-level loop via `overloads.into_iter().enumerate()`. No separate sort was needed: the
sequence's own `Vec<ResolvedCandidate>` construction order (outer loop over MRO levels, inner
loop over each level's stored overloads) already IS `(level, stored_idx)`-ascending by
construction — the fields exist as the queryable structural facts a future consumer (E9's
cursor) needs, not because today's code needs to re-derive the order from them.

**Build-time dedup.** `drop_flattened_role_duplicate_candidates`, a new private helper called at
the end of `resolve_sequence` (before the `ResolvedSequence` is returned), mirrors
`resolution_method.rs`'s `drop_flattened_role_duplicates` — dropping a composed role's own raw
MRO-level candidate once a class level already carries the role-flattened copy — but runs it
at sequence *build* time instead of the original's post-match-filter time. This is
behavior-preserving: the dedup removes by owner identity only, and the flattened copy that
survives has the same signature as the raw copy it replaces, so filtering before or after a
`method_args_match_for_invocant` pass yields the same final matched set either way.

**Ranker extraction.** `Interpreter::match_sequence_candidates` pulls the "filter a sequence's
`User` candidates by per-call signature match, producing `Vec<(Symbol, MethodDef)>`" loop out of
`shadow_check_resolver_chain` (E4a's own winner probe), so [`Interpreter::shadow_check_deferral_sequence`]
below can reuse it instead of carrying a second copy of the same loop. Ranking itself is
untouched — [`Interpreter::pick_method_winner`] is still the only ranker, called separately, and
by design decision 1 `level`/`stored_idx` do not feed it (they are deferral-order-only facts).

**The deferral-list shadow check.** `Interpreter::shadow_check_deferral_sequence`
(`resolution_sequence.rs`), gated behind `MUTSU_VM_STATS` like every prior Phase E probe, hooks
`Interpreter::push_method_dispatch_frame` (`accessors_state.rs`) — the single real call site
that builds the `nextsame`/`callsame` "remaining" deferral list, via `resolve_all_methods_with_
owner` + fingerprint-based winner removal, consumed by all six `push_method_dispatch_frame(...)`
call sites in `vm/vm_call_method_compiled*.rs`. The shadow computation: build the sequence for
`receiver_class`'s MRO, run it through `match_sequence_candidates` (invocant-BLIND — see finding
1 below), remove the caller's own already-computed winner fingerprint (mirroring
`push_method_dispatch_frame`'s exact loop shape: fingerprint-compare against the winner FIRST,
THEN the `should_skip_defer_method_candidate` hidden-parent filter, so a candidate that is both
the winner and nominally hidden is dropped for the winner reason only), and compare the
resulting `Vec<u64>` fingerprint list — order-sensitive, since deferral ORDER is user-observable
through repeated `nextsame` — against the real `remaining` list's own fingerprints, under a new
`DEFERRAL_SHADOW_CHECKS`/`_MISMATCHES` counter pair (`vm_stats.rs`), dedicated rather than
shared with `RESOLVER_SHADOW_*` for the same "comparing an ordered LIST, not a single winner
pick" reason E7 steps 4/6/7 already established for their own dedicated pairs. Same `where`-
clause care point as every prior probe: any candidate carrying a `where` clause anywhere in the
sequence skips the whole check, since re-running `method_args_match_for_invocant` a second time
would duplicate that clause's dynamic-variable side effects.

**"Shadow-compare winner AND deferral list" — the winner half needed no new code.** Per design
decision 1, `level`/`stored_idx` do not change winner ranking at all, so E4a's existing
`shadow_check_resolver`/`shadow_check_resolver_chain` at the two `resolve_method_cached`
boundaries already IS the winner-side shadow check the slice plan calls for; it now simply
exercises the enriched `ResolvedCandidate::User` shape (with `level`/`stored_idx` present but
unread by the winner ranker) for free. No new winner-probing call sites were added.

**Finding 1 (real bug in the new probe, fixed before landing): invocant-blind matching.** The
first version of `shadow_check_deferral_sequence` passed `Some(invocant)` to
`match_sequence_candidates`, reasoning that the real invocant should narrow the match. But the
REAL target it shadows — `resolve_all_methods_with_owner` — always calls
`method_args_match_for_invocant(..., invocant: None)` (a pre-existing property of that function,
unrelated to this box), so the deferral list is invocant-BLIND: it never checks `:U:`/`:D:`
smiley constraints, only the non-invocant argument shape. This mirrors raku's own semantics —
`nextsame`/`callsame` inside a `:U:`/`:D:` multi pair CAN walk to the sibling smiley candidate,
since the deferral list is not re-filtered by the invocant a second time. An invocant-aware
shadow probe is therefore *stricter* than the thing it is supposed to shadow, which produced
mismatches on every `::?ROLE:U:`/`::?ROLE:D:`-shaped test in the sweep
(`t/role-ud-multi-dispatch.t`: 6/6 mismatched → 0/6 after the fix;
`t/multi-method-invocant-definedness.t`: 6/6 → 0/6; `t/qualified-mu-coercion.t`: 4/4 → 0/4).
Switching the `match_sequence_candidates` call to `invocant: None` fixed all of them, dropping
the sweep's total mismatch count from 73 to 58 (see below). Documented at length in the
function's own doc comment so a future reader does not re-introduce the "more accurate must be
better" mistake.

**Finding 2 (pre-existing, accepted divergence — documented, not fixed): `method_entries`
never covers an un-punned role.** After fix 1, every remaining mismatch traced to one root
cause, confirmed by hand on all ten mismatching files: `resolve_sequence`'s per-level lookup
(`Registry::user_method_overloads`, reading the E1/E2 canonical `method_entries` table) silently
returns `None` for a role owner that has never been *punned* (`RoleName.new`, which briefly
registers — then, on withdrawal, un-registers — a synthetic `ClassDef` for the pun; see
`Registry::sync_user_method_entries`, which only ever reads `self.classes`). The real deferral
walker, `resolve_all_methods_with_owner`, has no such gap: it reads `self.registry().roles`
directly, bypassing `method_entries` entirely. So a role's own un-flattened method — reachable
whenever the class overrides the role's method with its own (`t/supply-nested-whenever-
emitter.t`, `t/multi-udismiley-ambiguity-leak.t`), when two role methods conflict and the class
resolves the conflict itself (`t/role-conflict.t`), or via a role-qualified call
`self.R::name()` (`t/qualified-method-call.t`) — is invisible to `resolve_sequence` but visible
to the real walker. Every one of the 58 remaining mismatches (46 files with checks, 160 total
checks) had the exact shape `real_len` one candidate ahead of `shadow_len`, confirmed on all ten
mismatching files (`t/anon-class-does-imported-role.t` 4/4, `t/builtin-distribution-role.t` 1/1,
`t/callsame-punned-role-and-hyper-infix-sub.t` 2/2, `t/multi-udismiley-ambiguity-leak.t` 18/12,
`t/qualified-method-call.t` 1/1, `t/role-conflict.t` 1/1, `t/role-required-method-name-based.t`
1/1, `t/role-required-universal-method.t` 3/3, `t/supply-nested-whenever-emitter.t` 1/1,
`t/yaml-battery.t` 39/32). **Not fixed inside E8a**: `get_method_overloads` (the same table) also
feeds several REAL production dispatch paths — `resolve_method_with_owner_impl` (winner
selection itself), `ctor_phase_plan.rs`, `vm_call_method_compiled_cache.rs`, and all three
`resolution_private_method.rs` call sites — so populating role entries there is a real
dispatch-behavior change (most likely a latent bugfix, masked for winner selection today by
early-stopping short-circuiting before reaching an un-punned role's MRO level in the common
case, but unverified for every call site), outside a shadow-only box's "zero real behavior
change" mandate. Root-caused, documented, and left with a suggested fix and verification plan in
`todo/deep/method-entries-never-covers-unpunned-roles.md` — a new, standalone finding, not
something that evaporates once E8a merges.

**Verification.** A `MUTSU_VM_STATS=1` sweep of the full local `t/` suite (3070 files) found 160
deferral-shadow checks across 46 files with any nextsame/callsame-shaped candidate, 58
mismatches, all attributed to finding 2 above (confirmed by hand on every mismatching file, not
inferred from the aggregate shape alone). A roast slice touching multi/role/submethod/wrap
dispatch — `roast/S06-advanced/{callsame,dispatching,wrap}.t`,
`roast/S06-multi/{redispatch,type-based,syntax}.t`,
`roast/S12-methods/{defer-call,defer-next,lastcall,multi,parallel-dispatch}.t`,
`roast/6.c/S12-class/mro-6c.t`, `roast/S12-class/{inheritance,basic}.t`,
`roast/6.c/S14-roles/mixin-6c.t` (16 files) — found 37 checks, 0 mismatches (the roast
whitelist's own corpus does not happen to exercise an un-punned role's nextsame/callsame
deferral path the way the hand-written `t/` regression tests do). Two new unit tests:
`resolve_sequence_assigns_level_and_stored_idx`,
`resolve_sequence_drops_a_flattened_role_duplicate_at_build_time`. `cargo build`/`cargo clippy
-- -D warnings`/`cargo fmt --check` clean; `cargo test --lib` (812 tests) and the full local
`make test` (3070 files / 28652 tests) green. `resolve_all_methods_with_owner`,
`push_method_dispatch_frame`'s own logic, and every real dispatch decision are untouched.

**Next E8 sub-slice: E8b — proto methods into `MethodEntry`**, per the slice plan above.

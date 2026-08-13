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

## E8b: proto methods gain a `MethodEntry` column in shadow mode; the registry's own sync logic was silently dropping proto-only rows

Landed 2026-08-12. Scoped down from this slice plan's original text ("proto methods into
`MethodEntry`; `lookup_proto_method` deleted") to a measure-first shape, matching E1a's own
precedent (`TypeId` landed beside the still-authoritative string owner; a later box made it
authoritative) rather than E8a's "shadow-check-then-immediate-cutover-in-the-same-PR" shape —
proto methods have exactly one write site and one read site (both far lower blast radius than
E1a's owner-string problem), but a *cutover* is still a different risk class from adding a
column even when the write site is singular, so the cutover itself is deferred to E8c.

**Structural change.** `MethodEntry` (`registry.rs`) gained `proto: Option<FunctionDef>`.
`Registry::set_proto_method(class_name, method_name, def)` is the single write site (called
from `registration_class_body.rs`'s `class_body_proto_method_decl`, replacing its old direct
`proto_methods.insert(...)`): it writes `def` into both the still-standalone `proto_methods:
HashMap<(String, String), FunctionDef>` (still the *only* table `Interpreter::
lookup_proto_method` reads for real dispatch) and the new `MethodEntry.proto` column, keeping
them in lockstep by construction. `Registry::method_entry_proto(class_name, method_name)` is
the new column's read side — a single-level probe (no MRO walk), mirroring
`user_method_overloads`'s own shape.

**Shadow check.** `lookup_proto_method`'s real MRO walk (`class_mro` + `proto_methods` lookup
per level) is untouched; it now also calls `shadow_check_proto_method` under
`MUTSU_VM_STATS`, which repeats the identical MRO walk reading `method_entry_proto` instead and
compares (owner name, `FunctionDef::body_fingerprint()`) against the real result — a dedicated
`PROTO_METHOD_SHADOW_CHECKS`/`_MISMATCHES` counter pair (`vm_stats.rs`), following the same
"one pair per probe family, not the shared `RESOLVER_SHADOW_*` infra" convention every prior
box in this ADR established.

**Finding (a real bug, in existing code the box merely lit up — fixed): `sync_user_method_
entries` was dropping proto-only rows.** The first sweep found *majority* mismatches (e.g.
10/13, 12/19 checks per file), always shaped `real=Some(owner) shadow=None` — the shadow
column had nothing where the real table had an entry. Root cause: `Registry::
sync_user_method_entries` (pre-existing, run from every one of `registration_class_body.rs`'s
own call sites *after* a proto decl in the same class body already landed, plus composition/
augmentation/redeclaration sites elsewhere) `retain`s a `(owner, name)` row only when
`entry.builtin.is_some() || !entry.user_candidates.is_empty() || entry.accessor.is_some()`. A
row holding only a freshly-written `.proto` matched none of those three and was dropped from
the map outright the moment anything else synced that owner — which, for a proto method
declared inside a class body, is *immediately*, since `class_body_proto_method_decl` runs
mid-body and later statements in the same body trigger further syncs before the class exits.
Fixed by adding `entry.proto.is_some()` to the retain's keep condition. `.proto` itself is
deliberately left OUT of the `key.owner == owner` clearing branch just above that condition
(the one that resets `user_candidates`/`accessor` before re-deriving them from `ClassDef`
below): unlike those two, `.proto` has no `ClassDef`-backed source to re-derive from — it is
written once, directly, only by `set_proto_method` — so clearing it there would delete it with
nothing to repopulate it. Confirmed zero real-behavior impact from the bug itself: nothing
outside this box's own new shadow probe read `.proto` before the fix landed, so the drop was
entirely self-contained to a column no real dispatch path had started consuming yet — exactly
the kind of "bug in the box's own new probe/scaffolding, not production" finding this ADR's
prior boxes (E7, E8a) also hit and fixed before landing.

**Verification.** After the fix, a `MUTSU_VM_STATS=1` sweep of every `t/` file mentioning
`proto method`/`proto submethod` (22 files) found 171 checks, 0 mismatches. A roast slice
touching proto/multi/wrap dispatch — the same 16-file list E8a's own verification used above
(`S06-multi/{proto,type-based,syntax,redispatch}`, `S12-methods/{defer-next,defer-call,
lastcall,multi,parallel-dispatch}`, `S06-advanced/{callsame,dispatching,wrap}`,
`6.c/S12-class/mro-6c`, `S12-class/{inheritance,basic}`, `6.c/S14-roles/mixin-6c}`, plus
`S06-multi/proto.t` added since it is the box's own dedicated roast file) — found 24 checks (3
files actually exercise a proto method under `MUTSU_VM_STATS`; the rest is coverage against a
false negative, not evidence of a gap), 0 mismatches. Two new unit tests: `set_proto_method_
populates_both_the_legacy_table_and_method_entries`, `method_entry_proto_is_scoped_to_the_
exact_owner`. `cargo build`/`cargo clippy -- -D warnings`/`cargo fmt` clean; `cargo test --lib`
(814 tests) and the full local `make test` (3071 files / 28661 tests) green.
`lookup_proto_method`'s own return value and every real proto-method dispatch decision are
untouched — `proto_methods` stays the sole table actually read for dispatch.

**Next E8 sub-slice: E8c — cutover.** With both sweeps at zero mismatches, `proto_methods` and
`lookup_proto_method`'s standalone MRO walk are ready to retire in favor of reading
`method_entries` directly (recovering the slice plan's original E8b scope), but that is left as
its own slice rather than folded into this one — matching E1a→E1b's two-step precedent, since a
cutover changes what real dispatch reads even when the measurement backing it is clean. After
E8c, the next Phase E box is **E9-pre**: the mandatory raku verification campaign for
`samewith`/`nextsame`/`callsame`/`nextwith` cursor semantics (design decision 3 above) — 拙速厳禁.

## E8c: proto-method cutover — `Registry::proto_methods` retired, `E8` closes

Landed 2026-08-12, immediately after E8b. Recovers the slice plan's original E8b text ("proto
methods into `MethodEntry`; `lookup_proto_method` deleted") now that E8b's shadow measurement
(171+24 checks, 0 mismatches across both a `t/` sweep and a roast slice) made the cutover a
same-answer swap rather than a guess, matching how E1b cashed in E1a's `TypeId` shadow column.

**The cutover itself.** `Interpreter::lookup_proto_method`'s real MRO walk (`dispatch_proto.rs`)
now calls `Registry::method_entry_proto` per MRO level directly — exactly the loop body E8b's
`shadow_check_proto_method` was already running, just promoted from shadow to real. The
standalone `proto_methods: HashMap<(String, String), FunctionDef>` field is deleted from
`Registry` outright, not kept as a secondary/debug store: `git grep -n "proto_methods" src/`
before the change showed exactly two readers (the real walk and its own shadow probe, both in
`dispatch_proto.rs`, both now gone or rewritten) and one writer (`Registry::set_proto_method`).
No `.^methods`/`.^lookup`/`.^find_method`/other MOP introspection call site ever read
`proto_methods` — those all resolve through `method_entries`'s `user_candidates`/`builtin`
columns for their own purposes and never needed a separate proto lookup — so there was no
"something else still legitimately needs it" case to preserve; the table was genuinely dead
once its two readers moved off it.

**Fast-path replacement.** The old `proto_methods.is_empty()` check let
`lookup_proto_method` skip the MRO walk entirely for the (common) case where no class in the
whole program has ever declared a proto method. Since `method_entries` has no single cheap
"any row has `.proto` set" query, this is replaced by a new field, `Registry::
has_proto_methods: bool`, flipped to `true` once by `set_proto_method` and never reset — proto
bodies are never unregistered in mutsu (no equivalent of removing a method from a class at
runtime), so a monotonic flag is sound and cheaper than a counter or a second set.

**Probe teardown.** `shadow_check_proto_method` and the `PROTO_METHOD_SHADOW_CHECKS`/
`_MISMATCHES` counter pair (`vm_stats.rs`), including their `adr0019-e8b` stats-report lines,
are deleted. This mirrors E1b's own teardown of E1a's probes at each site E1b cut over: once
the shadow answer IS the real answer, comparing them is comparing a value to itself.

**Tests.** Two `registry.rs` unit tests updated: `set_proto_method_populates_both_the_legacy_
table_and_method_entries` renamed to `set_proto_method_populates_method_entries_and_the_fast_
path_flag` and rewritten to assert against `method_entries`/`has_proto_methods` instead of the
retired table (plus a new assertion that the flag starts `false` and flips `true`);
`method_entry_proto_is_scoped_to_the_exact_owner` unchanged (it only ever read the new column).
No new `t/` test: this is a same-answer read-path swap with nothing new to pin, and the existing
proto-method suite already exercises plain-class, inherited, and role-composed proto shapes
end to end (10 files, 72 assertions: `t/proto-method-body.t`, `t/proto-method-rw-redispatch.t`,
`t/proto-cross-module-invocant.t`, `t/handles-proto-dispatch-mut-invocant.t`, `t/proto-multi-
captured-writeback-coherence.t`, `t/proto-new-no-match.t`, `t/proto-multi-method-role-
composition.t`, `t/multi-udismiley-ambiguity-leak.t`, `t/role-ud-multi-dispatch.t`, `t/qualified-
mu-coercion.t`) and stayed green through the cutover.

**Verification.** `cargo build`/`cargo clippy -- -D warnings`/`cargo fmt --check` clean; `cargo
test --lib` (814 tests, same count as E8b — one rename, no net add/remove) and the full local
`make test` (3074 files / 28683 tests) green. Per CLAUDE.md's "touched name/type resolution"
rule — this changes a real dispatch read path even though the risk was already retired by
measurement — a local roast slice (the same 16-file list E8a/E8b used:
`S06-multi/{proto,type-based,syntax,redispatch}`, `S12-methods/{defer-next,defer-call,lastcall,
multi,parallel-dispatch}`, `S06-advanced/{callsame,dispatching,wrap}`, `6.c/S12-class/mro-6c`,
`S12-class/{inheritance,basic}`, `6.c/S14-roles/mixin-6c`) found 524 assertions, all green.

**E8 closes.** E8a + E8b + E8c together deliver everything this box's own text scoped:
candidates carry `level`/`stored_idx` so winner selection and deferral order both derive from
one sequence (E8a); `Registry::proto_methods` has folded into `MethodEntry` and the standalone
table is gone (E8b structural + E8c cutover); the method-vs-sub ranking ladders stayed
deliberately unmerged, as designed from the start. The one item E8a's own sweep found but did
NOT fix — `resolve_sequence`'s per-level lookup silently missing an un-punned role's own method
(`todo/deep/method-entries-never-covers-unpunned-roles.md`) — remains open, but lives in a
different lookup path (`user_method_overloads`) that neither E8b nor E8c touched; it is not part
of E8's own closing scope.

**Next Phase E box: E9-pre.** The mandatory raku verification campaign for `samewith`/
`nextsame`/`callsame`/`nextwith` cursor semantics (design decision 3 above), flagged as the
highest-semantic-risk box of the whole phase (拙速厳禁). It needs its own dedicated session —
not a tail slice bolted onto E8c — and must land before any E9a/b/c cursor-cutover work starts.

## E9-pre: the raku ground-truth campaign ran — 12 pins, 8 divergence findings, and design decision 2 is AMENDED

Landed 2026-08-12 as its own dedicated session, exactly per decision 3's mandate: every scenario
was probed against real raku (Rakudo v2026.06) FIRST, matching behaviors were pinned as `t/`
tests (each pin verified to pass under BOTH `prove -e raku` and `prove -e target/debug/mutsu` —
so the pins provably encode raku's answer, not mutsu's), and every divergence became a ticket —
no divergence was encoded into cursor semantics, and no cursor code was written.

### Scenario table (a-m from decision 3, plus bonus probes)

| # | scenario | verdict | artifact |
|---|----------|---------|----------|
| a | nextsame/callsame through multi candidates in one class; no-next → Nil; post-nextsame code unreachable | MATCH | `t/defer-multi-single-class.t` |
| b | callsame/nextsame through inherited plain methods (3 levels); top-of-chain → Nil | MATCH | `t/defer-inherited-chain.t` |
| c | `does`-composed role method overridden by the class: raku EXCLUDES it from the chain (plain and same-sig multi both get Nil); mutsu walks the role's raw copy | DIVERGE | `todo/tickets/role-shadowed-method-in-defer-chain.md` |
| d | `is Array` subclass `push` override → nextsame/callsame to native push: raku appends + returns self; mutsu appends nothing, callsame returns Any | DIVERGE | `todo/tickets/native-array-push-defer-fallback-broken.md` |
| e | Grammar `.parse` override: callsame/nextsame reach the real parse, Match flows back | MATCH | `t/grammar-parse-override-defer.t` |
| f | callsame in a method wrapper; double wrap = newest outermost | MATCH | `t/method-wrap-callsame-order.t` |
| f' | …but method-wrap REMOVAL: `$handle.restore` silently no-ops, `.unwrap($h)` throws | DIVERGE | `todo/tickets/method-wrap-unwrap-restore-noop.md` |
| g | wrap on ONE multi candidate (`.candidates[0].wrap`) scopes to that candidate; declaration-order candidate list | MATCH | `t/wrap-multi-candidate-scope.t` |
| h | mid-MRO wrap: child's callsame enters the parent's wrapper first; wrap-on-child composes wrapper → child body → parent | MATCH | `t/wrap-mid-mro-callsame.t` |
| i | `lastcall` inside a wrapper then callsame: raku → Nil (original never runs); mutsu dies "callsame is not in the dynamic scope of a dispatcher" | DIVERGE | `todo/tickets/lastcall-in-wrapper-callsame-dies.md` |
| i' | `lastcall` then nextsame in a plain multi → Nil, nothing else runs | MATCH | `t/lastcall-then-nextsame.t` |
| j | samewith restarts from the top with new args, incl. from a nextsame-reached candidate | MATCH | `t/samewith-restart-from-top.t` |
| k | callwith/nextwith keep `is rw` containers live through re-binding; callwith advances (not restarts) | MATCH | `t/callwith-rw-passthrough.t` |
| l | EXPLICIT proto in a child: `{*}` does NOT assume parent candidates (raku: X::Multi::NoMatch; mutsu: resolves the parent's) | DIVERGE | `todo/tickets/explicit-child-proto-assumes-parent-candidates.md` |
| l' | proto in the PARENT governs child-added candidates; nextsame under an explicit proto | MATCH | `t/proto-star-cross-mro-candidates.t` |
| m | BUILDALL parent-first; callsame inside a BUILD submethod → Nil | MATCH | `t/build-callsame-nil.t` |
| + | multi child ↔ plain parent cross-level deferral (both directions) | MATCH | `t/defer-multi-plain-cross-level.t` |
| + | **multi candidates at BOTH levels: chain order** (see below) | **DIVERGE** | `todo/deep/defer-chain-ranked-multi-order.md` |
| + | callsame from overrides of built-in Mu methods (gist/Str/raku/new) → raku reaches the native impl; mutsu gets Nil/Any | DIVERGE | `todo/tickets/callsame-to-native-mu-methods-nil.md` |
| + | Signature.gist invocant rendering `(C $:: ...)` vs `(C:, ...)` (cosmetic) | DIVERGE | `todo/tickets/signature-gist-invocant-format.md` |

12 pin files, 38 assertions, all green under raku AND mutsu; `runtime mixin (but R)` was also
spot-checked (match; already covered by `t/nextsame-role-mixin.t`).

### THE headline finding — design decision 2 is amended

Decision 2 above says the cursor should advance "applying the per-call signature filter as they
go (matching today's 'remaining = signature-matching candidates in MRO order' semantics)".
**That target is wrong: today's mutsu order itself diverges from raku whenever multi candidates
span MRO levels.** raku's model (full derivation and probes in
`todo/deep/defer-chain-ranked-multi-order.md`) is two-level:

- The outer chain is per-class entries along the MRO (plain method or proto).
- An IMPLICIT proto clones the nearest MRO proto and merges parent candidates into one
  **specificity-ranked** list (MRO breaks ties); nextsame/callsame walk that ranked list first
  and fall to the outer chain's next per-class entry only when it is exhausted. A plain method
  in a middle MRO level is NOT part of the ranked list — it is a later outer-chain entry, and
  deferring from it re-enters protos below it (so a parent multi candidate can legitimately run
  twice in one call).
- An EXPLICIT proto declared in a child does NOT assume parent candidates (finding l).

Consequences for E9a/b/c: the `DispatchCursor`'s sequence for the multi portion must be the
ranked merged list plus outer-chain fall-through (not the E4 `ResolvedSequence`'s flat
`(level, stored_idx)` order), and `samewith`'s "re-run the ranker over the same sequence" in
decision 2 stays correct as written. **E9a must not start until the cursor design is re-drawn
against `defer-chain-ranked-multi-order.md`** — that re-draw is a design task (amend decision 2
in this doc with the concrete sequence layout), not an implementation detail to discover
mid-cutover.

### Interaction with the E8a accepted divergence

E8a's 58 accepted deferral-shadow mismatches were attributed to `method_entries` missing
un-punned role owners, treating the REAL walker (`resolve_all_methods_with_owner`, which reads
`registry().roles` directly) as authoritative. Finding (c) shows that for the
class-overridden-role shape, raku agrees with the SEQUENCE side (role method absent), not the
real walker. The reconciliation note is in `todo/tickets/role-shadowed-method-in-defer-chain.md`
and cross-linked from `todo/deep/method-entries-never-covers-unpunned-roles.md`'s fix plan: that
sweep's mismatch ledger must be re-audited against raku per-shape, not resolved wholesale toward
the real walker.

## E9 design decision 2 — REDRAWN: the flat deferral expansion (confirmed by prediction)

Immediately after the campaign PR (#6325) two follow-up probes CONFIRMED a concrete replacement
for decision 2's sequence layout — each probe's chain order was predicted from the model BEFORE
running raku, and both predictions were exact hits. This section supersedes the "matching
today's 'remaining = signature-matching candidates in MRO order' semantics" clause of decision
2 above; everything else in decision 2 (one cursor struct, wrap prefix entries, native tail
entries, `samewith` = re-rank, `lastcall` = `next := len`, frame-by-frame E9a/b/c migration)
stands as written.

**The deferral expansion.** For a call of `name` on a receiver whose MRO is `K0, K1, ...`:

```
DeferralSequence(receiver, name) =
  concat over MRO classes K that install an entry for `name`:
    - plain method (or submethod, with the existing level-0 visibility rule): [ Method(K) ]
    - proto (explicit or implicit): [ the proto's RANKED candidate block ]
```

- An IMPLICIT proto at K (a `multi method` declaration with no proto at K) clones the nearest
  proto above K in the MRO and merges K's own candidates into it; the block is ranked by
  narrowness, with MRO depth then declaration order breaking ties. An EXPLICIT proto's block
  contains only its own class's candidates
  (`todo/tickets/explicit-child-proto-assumes-parent-candidates.md`).
- The SAME candidate may appear in several blocks (its own class's block plus every
  descendant's merged block). This is CORRECT, not a dedup bug: deferral runs it once per
  occurrence (probe 1 below shows a parent candidate legitimately running twice in one call).
  A flat cursor index over this expansion reproduces raku exactly — the cursor mechanics of
  decision 2 need no two-level structure; only the sequence BUILDER changes.
- Winner selection = the existing ranker over the receiver's nearest entry; the cursor starts
  immediately after the winner's occurrence in the expansion.
- Advancement applies the per-call signature filter (invocant-blind, per E8a finding 1) with
  the CURRENT args — original args for `callsame`/`nextsame`, replacement args for
  `callwith`/`nextwith`. The filter must be raku-strict: mutsu's matcher used to admit Int
  for a `Num $x` candidate and called a candidate raku's dispatcher skips (found by probe 2
  dying mid-chain). **Fixed same day** — the Int/Rat→Num "numeric widening" was removed from
  the shared matcher and binder (`news/2026-08/multi-num-param-strictness.md`, pinned by
  `t/multi-num-param-strictness.t` including the deferral-skip shape), so this E9a
  prerequisite is retired.
- Role composition: the class-level entry carries the flattened copies; a `does`-composed
  role's own MRO appearance contributes NO entries when the class overrides the method
  (`todo/tickets/role-shadowed-method-in-defer-chain.md`).

**Confirming probes** (inlined because `tmp/` is gitignored; predictions made before running):

Probe 1 — exhausting the merged block falls to the parent proto's OWN block, re-running its
candidate:

```raku
class P { multi method m(Int $x) { say "P:Int"; nextsame; say "P:u" } }
class C is P {
    multi method m(Int $x) { say "C:Int"; nextsame; say "C:u" }
    multi method m(Any $x) { say "C:Any"; nextsame; say "C:A-u" }
}
C.new.m(1)
# expansion: C-block[C:Int, P:Int, C:Any], P-block[P:Int]
# predicted & observed (raku): C:Int, P:Int, C:Any, P:Int, Nil
# mutsu today:                 C:Int, C:Any, P:Int, Nil
```

Probe 2 — three-level implicit-clone chain, strict per-call filter during advance:

```raku
class A { multi method m(Int $x) { say "A:Int"; nextsame; say "A:u" } }
class B is A { multi method m(Str $x) { say "B:Str"; nextsame; say "B:u" } }
class C is B { multi method m(Num $x) { say "C:Num"; nextsame; say "C:u" } }
C.new.m(1)
# expansion for arg 1 (Str and Num candidates filtered out — 1 !~~ Num in raku):
#   C-block[A:Int], B-block[A:Int], A-block[A:Int]
# predicted & observed (raku): A:Int, A:Int, A:Int, Nil
# mutsu today: A:Int, then DIES X::TypeCheck::Binding::Parameter calling the Num candidate
```

**E9a discipline inversion.** Unlike every earlier Phase E box, E9a cannot prove itself by
shadow-comparing against the real walker: the real walker is WRONG wherever the E9-pre tickets
point. E9a is therefore a deliberate behavior-CHANGING cutover justified by (i) the E9-pre pins
staying green, (ii) NEW pins for the flipping shapes (both-levels-multi order, role-shadowed
exclusion, explicit-proto isolation, probes 1/2 above) written with raku's expected values in
the same PR, and (iii) a local `make roast` per the house rule for dispatch-semantics changes.
The matcher-strictness ticket is a prerequisite or co-requisite: without it the stricter
advance filter cannot be expressed.

**Progress 2026-08-12 (same day) — E9a landed for the both-levels-multi-order shape only; the
DispatchCursor struct itself is deferred.** `src/runtime/resolution_deferral.rs` adds
`Interpreter::resolve_deferral_expansion`, a new ordering source that replaces
`resolve_all_methods_with_owner` at the two "remaining"-building call sites
(`accessors_state.rs::push_method_dispatch_frame`, `class_dispatch.rs`'s `build_remaining`
closure): it builds the flat per-MRO-class expansion described above (implicit-clone-merge
ranked by nominal narrowness/MRO-depth/decl-order, explicit-proto isolation) instead of a bare
per-level declaration-order walk. Both probes 1 and 2 above are exact hits against Rakudo
v2026.06 (verified with `raku` directly, not just predicted) and pinned in
`t/defer-multi-cross-level-proto-block.t`; all 12 E9-pre pins plus the full `multi`/`nextsame`/
`callsame`/`wrap`/`proto`/`defer`/`samewith` corner of `t/` (148 files) stay green.

Two scope decisions, both deliberate:
- **The winner-removal mechanism (fingerprint-compare-and-skip) is UNCHANGED** — only its input
  ordering changed, from `resolve_all_methods_with_owner`'s output to
  `resolve_deferral_expansion`'s. This is a smaller, lower-risk slice than decision 2's full
  `DispatchCursor{seq, next, invocant, args}` (index-based, no re-walk) — that mechanical
  refactor is orthogonal to the *ordering* fix that raku ground truth actually demands and is
  left for a follow-up (`MethodDispatchFrame` still carries a re-derived `Vec`, not an `Arc`
  sequence + cursor index). No observable behavior depends on which storage shape is used.
- **`role-shadowed-method-in-defer-chain.md` and `explicit-child-proto-assumes-parent-
  candidates.md` remain OPEN, not fixed by this slice.** The role-shadow ticket needs
  `resolve_deferral_expansion` (or its shared `own_overloads_at_level` read) to also drop a
  role's raw MRO entry when a class level shadows it with an independently-authored override —
  a distinct fix from the ordering redraw, not implied by decision 2. The explicit-proto ticket
  is about the `{*}` re-entry itself (`dispatch_proto_call.rs` re-entering
  `call_method_with_values` by name, which still walks the OLD `resolve_method_with_owner_impl`
  MRO walker for the *winner*, unaffected by this box) — that is E9c's job per decision 2's own
  migration order. Filing new pins for either shape now would encode a still-open mutsu bug as
  a passing test; both tickets stay as-is.

## E9b design (2026-08-13): method wraps become deferral-frame prefix entries; the wrap stack reverts to sub-only

Design pass only — no code. Surveyed against current main (`bd9a94acd`, which includes the
#6349 lastcall-in-wrapper fix and the #6355 explicit-proto boundary). Two raku probes run
during this pass each found a REAL divergence in the existing wrap machinery (inlined below,
per the E9-pre convention, because `tmp/` is gitignored); both are fixed *structurally* by
this design, and both are also filed as tickets so they cannot evaporate:
`todo/tickets/wrap-chain-skipped-inside-foreign-wrap-dispatch.md` and
`todo/tickets/callsame-in-method-consumes-enclosing-sub-wrap-chain.md`.

### Facts (updated line refs; the §"Facts" survey at the top of this doc has drifted)

- The two frames carry disjoint payloads: `WrapDispatchFrame { sub_id: u64, remaining:
  Vec<Value>, args: Vec<Value>, arg_sources: Option<Vec<Option<String>>> }`
  (`decl_types.rs:178-193`) vs `MethodDispatchFrame { receiver_class, invocant, args,
  remaining: Vec<(String, MethodDef)>, rw_params }` (`decl_types.rs:165-176`). The type
  mismatch in `remaining` (code objects vs owner+def pairs) is the representational problem
  this box solves. `wrap_dispatch_stack` lives at `mod.rs:1910-1911`.
- Four `WrapDispatchFrame` construction sites: the sub wrap (`resolution_call_sub.rs:225-291`
  — the only `sub_id != 0` site), the interpreter method-wrap entry
  (`class_dispatch.rs:336-420`), the VM method-wrap entry (`check_method_wrap_chain`,
  `vm_call_method_compiled.rs:271-402`), and the mid-MRO interception inside
  `dispatch_next_candidate` itself (`builtins_dispatch_next.rs:487-543`).
- The wrap prefix and the MRO deferral list are two cursors spliced at runtime by THREE
  mechanisms, all of which this box deletes: (i) the `sub_id == 0` method-wrap sentinel and
  its exhaustion fallthrough (`builtins_dispatch_next.rs:414`, `:452-469`), including the
  #6349 `wrap_chain_exhausted` bool threaded to the final fallback (`:400-404`, `:468`,
  `:967`); (ii) the synthetic "original" sub tagged `__mutsu_method_wrap_original`
  (`class_dispatch.rs:370-380`, `vm_call_method_compiled.rs:294-303`) whose advance leg
  re-enters `call_method_with_values` BY NAME (`builtins_dispatch_next.rs:425-433`), guarded
  against chain re-entry by the global `is_inside_wrap_dispatch()` checks at both entry sites
  (`class_dispatch.rs:341`, `vm_call_method_compiled.rs:282-284`); (iii) the mid-MRO
  peek-and-intercept block (`:487-543`), which pushes a wrapper-only frame (no synthetic
  original) and deliberately does NOT consume the peeked candidate.
- E9a's `resolve_deferral_expansion` (`resolution_deferral.rs`) is wrap-blind by construction
  — zero occurrences of wrap logic. Wrap is a fully separate stack layered on top.
- A plain (single, non-multi, non-wrapped-by-MRO) wrapped method pushes NO
  `method_dispatch_stack` frame at all (`push_method_dispatch_frame`'s `<=1` fast-outs,
  `accessors_state.rs:788-796`, `:822-827`) — the situation the #6349 bool exists to paper
  over.
- `wrap_skip_once` (`mod.rs:1912-1918`) is set at exactly one place
  (`builtins_dispatch_next.rs:442-444`) and consumed at exactly one
  (`resolution_call_sub.rs:230`); it is sub-side only and is untouched by this box.

### Probe findings (both raku-confirmed divergences in TODAY's machinery)

**P1 — a wrapped method called from inside a FOREIGN wrapper loses its own chain.** The
global `is_inside_wrap_dispatch()` guard at both method-wrap entry sites suppresses every
wrap chain while ANY wrap dispatch is live, including a different method's:

```raku
class A { method x() { "x-orig" } }
class B { method y() { "y-orig" } }
A.^lookup('x').wrap(-> $self { "x-wrap[" ~ callsame() ~ "]+" ~ B.new.y });
B.^lookup('y').wrap(-> $self { "y-wrap[" ~ callsame() ~ "]" });
say A.new.x;
# raku:  x-wrap[x-orig]+y-wrap[y-orig]
# mutsu: x-wrap[x-orig]+y-orig          (B's chain silently skipped)
```

The guard exists only to stop the synthetic original's by-name re-entry from re-entering its
own chain; it is far too blunt. Ticket:
`todo/tickets/wrap-chain-skipped-inside-foreign-wrap-dispatch.md`.

**P2 — `callsame` in a method dispatched from inside a sub's wrapper consumes the SUB's wrap
chain.** `dispatch_next_candidate` searches the stacks in fixed priority wrap → method
(`builtins_dispatch_next.rs:403-406`), so a live sub-wrap frame shadows a more-recent method
frame:

```raku
class P { method m() { "P-m" } }
class C is P { method m() { "C-m[" ~ callsame() ~ "]" } }
sub g() { "g-orig" }
&g.wrap(sub () { say C.new.m; "g-wrap[" ~ callsame() ~ "]" });
say g();
# raku:  C-m[P-m]    then g-wrap[g-orig]
# mutsu: C-m[g-orig] then "Use of Nil in string context" + g-wrap[]
#        (the method's callsame ate g's chain; g's own callsame then found nothing)
```

Ticket: `todo/tickets/callsame-in-method-consumes-enclosing-sub-wrap-chain.md`.

### Design decisions

**1. `DeferralEntry` — the frame's `remaining` becomes heterogeneous.**

```rust
pub(crate) enum DeferralEntry {
    /// A wrapper code object; invoked with [invocant, args...] and shifted arg sources.
    Wrapper(Value),
    /// A user method candidate; invoked directly as a resolved method (the existing
    /// method-frame advance leg, builtins_dispatch_next.rs:542-770, unchanged in substance).
    Candidate { owner: Symbol, def: MethodDef, wraps_spliced: bool },
}
```

`MethodDispatchFrame.remaining` becomes `Vec<DeferralEntry>`, and the frame gains
`arg_sources: Option<Vec<Option<String>>>` (today carried only by the wrap frame; needed by
the Wrapper leg's rw-param source restoration, `builtins_dispatch_next.rs:420-424`).
`gc_roots.rs` must trace `Wrapper` values (today `:153-156` traces the wrap frames' `remaining`;
that tracing moves with the values).

**2. One frame per wrapped method call; the wrap stack no longer holds method wraps.** The two
method-wrap entry sites build a SINGLE `MethodDispatchFrame` whose `remaining` is
`[Wrapper(below-outermost wrappers, in call order)..., Candidate(winner, wraps_spliced:
true), ...MRO tail from resolve_deferral_expansion...]`, then invoke the outermost wrapper
directly (mirroring today's "outermost runs, the rest are `remaining`" shape). Consequences:

- A wrapped PLAIN method now always gets a frame — the wrap entry path bypasses
  `push_method_dispatch_frame`'s `<=1` fast-outs whenever a wrap prefix exists. Exhaustion
  becomes "frame exists, `remaining` empty → Nil", which deletes the #6349
  `wrap_chain_exhausted` bool with no replacement state.
- Deleted outright: the `sub_id == 0` sentinel and its exhaustion fallthrough, the synthetic
  original sub and the `__mutsu_method_wrap_original` marker, the by-name original re-entry
  leg, and the global `is_inside_wrap_dispatch()` guards at the method entry sites (the
  original is now invoked directly as a resolved `Candidate`, so there is no by-name re-entry
  to protect — and deleting the guard is precisely the P1 fix; a nested call to a different
  wrapped method enters its own chain like any fresh dispatch).
- `WrapDispatchFrame` survives for SUB wraps only (`resolution_call_sub.rs` site untouched;
  `sub_id` is now always a real id — add a `debug_assert!(sub_id != 0)` at the push helper).
  `wrap_skip_once` stays as-is (sub-side). Sub wrap exhaustion still returns Nil.
- `lastcall` inside a method wrapper truncates the unified frame's `remaining` — same
  observable Nil-on-callsame as #6349 pinned, one mechanism instead of two.
  (`t/lastcall-in-wrapper-callsame-dies.t` must stay green; the still-open
  `lastcall-in-wrapper-nextsame-swallows-output.md` ticket — the `nextsame` routine-boundary
  unwind divergence — is OUT of this box's scope, but the unified frame is its prerequisite
  groundwork.)

**3. Mid-MRO wraps: lazy splice at advance, not build-time expansion.** Decision 2's original
phrasing ("resolving a wrapped candidate yields [Wrapper..., candidate, ...rest]") reads as
build-time expansion; this design deliberately amends it to LAZY: when advancement reaches a
`Candidate { wraps_spliced: false }` whose `(owner, method, find_method_candidate_index)` has
a chain (same lookups the interception block does today, `builtins_dispatch_next.rs:499-507`),
replace that entry in place with `[Wrapper(chain)..., Candidate { wraps_spliced: true }]` and
advance into the first Wrapper. Two reasons, in the repo's gain/risk terms:

- *Timing parity*: today the mid-MRO chain is read at advance time (`:487-543`), so a
  `.wrap`/`.unwrap` executed mid-dispatch is honored; build-time expansion would silently
  change when chains are observed — an unverified semantic difference with no raku ground
  truth taken, i.e. exactly the kind of by-value-style "correct only if nothing mutates"
  choice the CLAUDE.md risk definition warns against.
- *Cost*: build-time expansion pays `find_method_candidate_index` (Arc::ptr_eq, then an
  O(candidates × AST) fingerprint fallback — `accessors_state.rs:907-940`) for every tail
  candidate on every wrapped dispatch, even though most deferral lists are never advanced.

The winner's own prefix IS built at frame-build time (that moment is the advance for the
winner), with `wraps_spliced: true` so it is never re-expanded. The entire interception block
(`:470-543`) is deleted.

**4. Cross-stack frame ordering: a shared dispatch token replaces fixed priority (the P2
fix).** `wrap_dispatch_stack` (sub wraps), `method_dispatch_stack`, and
`multi_dispatch_stack` frames each gain a `dispatch_token: u64` stamped from one shared
monotonic counter at push. `dispatch_next_candidate`, `builtin_lastcall`, and
`builtin_nextcallee` select the live frame with the HIGHEST token — i.e. the innermost
dynamic dispatch context — instead of the fixed wrap → method → multi search order
(`builtins_dispatch_next.rs:403`, `:58-77`, `:979-1035`). For today's paired method-wrap
frames the wrap frame is pushed second and would win, so the current pairing behavior is
preserved by construction; the only behavior change is the P2 shape, where the innermost
context is now correctly preferred. Exhaustion semantics stay per-family (an exhausted
sub-wrap frame still answers Nil; it does not fall through to an unrelated outer method
frame).

**5. Explicitly out of scope** (each has its own home): the method-wrap `unwrap`/`restore`
no-op (`todo/tickets/method-wrap-unwrap-restore-noop.md` — E10a, registry-owned wrap state);
the `has_any_wrap_chains()` prefilter and its five call sites (E10b, after E3); the
`lastcall`-then-`nextsame` routine-boundary unwind
(`todo/tickets/lastcall-in-wrapper-nextsame-swallows-output.md` — needs its own raku
verification pass); wrap identity's `find_method_candidate_index` fingerprint fallback and the
`(String, String, usize)` chain key (E10a); the sub-side `multi_dispatch_stack`'s own
`Vec`-advance mechanics (Phase C/F); `nextcallee` for method dispatch (today unimplemented,
`builtins_dispatch_next.rs:1000-1001` — note that `Wrapper` entries make it implementable
later by peeking the next entry, but do not implement it here).

### Slice plan

- **E9b-0** — the dispatch-token frame ordering (decision 4). Independent of the enum work,
  fixes P2 today, smallest possible diff (one counter, three stamped fields, three selection
  sites). Pin: the P2 probe as a raku-valued `t/` test. Local `make roast` (dispatch
  semantics).
- **E9b-1** — mechanical: `DeferralEntry` + `Vec<DeferralEntry>` + the `arg_sources` frame
  field; every existing builder emits `Candidate`; zero behavior change; full `t/` + clippy.
- **E9b-2** — the cutover, one coherent PR: single-frame construction at both method-wrap
  entry sites, the Wrapper advance leg, lazy splice, and the deletion list from decision 2/3
  (sentinel, marker, synthetic original, by-name leg, global guard, mid-MRO block,
  `wrap_chain_exhausted`). Fixes P1; pin the P1 probe raku-valued in the same PR. Gate: the
  full wrap/defer pin corpus (`t/wrap*.t`, `t/method-wrap-*.t`,
  `t/lastcall-in-wrapper-callsame-dies.t`, `t/lastcall-then-nextsame.t`, `t/defer-*.t`,
  `t/nextsame-role-mixin.t`), local `make roast`, and the E8a/E8b roast slice list plus
  `S06-advanced/wrap.t`.

### Risk notes

The closure-env write-back filters at both entry sites (`class_dispatch.rs:405-414`,
`vm_call_method_compiled.rs:346-395`) fire around the OUTERMOST wrapper call and must keep
doing so — the unified frame changes who owns `remaining`, not the entry bracket; advance-leg
`call_sub_value` calls on `Wrapper` entries do not run entry write-back today and must not
start to. Thread fork keeps its current shape (`wrap_dispatch_stack` forks empty,
`runtime_thread.rs:685`; the method stack likewise). The frame-priority change (E9b-0) is the
one slice that can change behavior outside wrap-using programs — any program nesting a method
deferral inside a sub wrapper — which is exactly why it lands first, alone, raku-pinned.

## E9c design (2026-08-13): proto `{*}` resolves directly within the governing boundary; `samewith`'s by-name restart is CONFIRMED correct and stays

Design pass only — no code. Probed against Rakudo v2026.06 first (拙速厳禁); the probes
settled the one semantic question this box hinged on, and found one adjacent real divergence
(ticket filed, deliberately NOT folded into this box's scope:
`todo/tickets/proto-method-body-skipped-for-type-object-invocant.md`).

### Probe results (inlined; run against both engines)

**P3 — `samewith` re-runs the governing proto BODY. mutsu already matches raku, in both the
method and the sub case.**

```raku
class C {
    proto method m($x) { say "proto($x)"; {*} }
    multi method m(Int $x) { say "int($x)"; samewith($x + 10) if $x < 10; }
    multi method m(Str $s) { say "str($s)" }
}
C.new.m(1);
# raku AND mutsu: proto(1) / int(1) / proto(11) / int(11)

proto sub f($x) { say "proto($x)"; {*} }
multi sub f(Int $x) { say "int($x)"; samewith("s") if $x != 0; }
multi sub f(Str $s) { say "str($s)" }
f(1);
# raku AND mutsu: proto(1) / int(1) / proto(s) / str(s)
```

**This falsifies decision 2's `samewith` clause** ("re-runs the *ranker* over the same
sequence with new args") for every proto-governed candidate: a sequence-level re-rank would
skip the proto body, whose side effects observably re-run. `samewith` is a full DISPATCHER
restart, and mutsu's by-name re-entry (`builtin_samewith`,
`builtins_dispatch_next.rs:101-118` → `call_method_with_values`/`call_function`) is the
CORRECT encoding of that semantics, not a legacy shortcut to remove. Decision 2 is amended
accordingly: E9c does not convert `samewith` to sequence-based re-ranking. A pin encoding P3
(`t/samewith-proto-body-rerun.t`, green on both engines today) lands with E9c-1.

**P4 — mutsu skips the proto body entirely for a TYPE-OBJECT invocant (real divergence,
adjacent, not this box).**

```raku
class P {
    proto method m($x) { say "proto($x)"; {*} }
    multi method m(Int $x) { say "int($x)" }
}
P.m(5);
# raku:  proto(5) / int(5)
# mutsu: int(5)            (proto body never runs)
```

Root cause is the `ValueView::Instance` gate at the interception entry
(`try_proto_method_body`, `dispatch_proto.rs:317-320`) — the interception, not the `{*}`
handler this box rewrites. Filed as
`todo/tickets/proto-method-body-skipped-for-type-object-invocant.md`; E9c-2 neither needs it
nor fixes it, but whoever fixes it will touch the same file.

### Facts

- `{*}` is rewritten at declaration to `__PROTO_DISPATCH__()` (`dispatch_proto_rewrite.rs`);
  the VM arm delegates the METHOD case to the interpreter unconditionally
  (`vm_call_func_ops.rs:1747-1749`), so this box's changes are confined to the interpreter.
- The method branch of `call_proto_dispatch` (`dispatch_proto_call.rs:5-75`): reads the
  `proto_dispatch_stack` frame (`(proto_name, args, Some(ProtoMethodCtx))`, pushed by
  `run_proto_method`, `dispatch_proto.rs:278-286`), re-derives the owner via a second
  `lookup_proto_method` walk (`:42-43`), rebuilds rw-mutated args from the proto body's live
  params (`proto_rw_redispatch_args`, `:44-49` — "part A"), sets the one-shot
  `proto_method_skip` (`:50`), restores pending call-arg sources so an `is rw` candidate can
  bind through the re-entry (`:51-60` — "part B"), brackets
  `proto_redispatch_boundary: Option<(Symbol, Symbol)>` (save/restore, `:61-73`), and
  re-enters `call_method_with_values` BY NAME (`:72`).
- The re-entry runs the full dispatch pipeline again: `try_proto_method_body` consumes the
  skip flag (`dispatch_proto.rs:321-324`), then `resolve_method_with_owner_impl` truncates
  its MRO walk at the boundary owner (`resolution_method.rs:135-144`); the diagnostics
  listing reads the boundary too (`format_method_candidate_signatures`, `class.rs:310-319`).
- The boundary bracket spans the CANDIDATE BODY's entire execution (the bracket closes after
  `call_method_with_values` returns), relying on (a) name-matching to not bite unrelated
  dispatches and (b) nested redispatches saving/restoring. No concrete mis-truncation repro
  was found during this pass (the shapes that would bite are themselves proto-governed and
  re-bracket correctly), but ambient dispatch state live across arbitrary user code is a
  standing hazard class, and it costs a save/restore plus a name-compare on every method
  resolution.
- The winner's deferral list is built UNtruncated (`class_dispatch.rs:424` build_remaining →
  `resolve_deferral_expansion`), which is correct only because E9a's expansion encodes
  governing-block isolation independently — two mechanisms expressing one rule.
- The VM multi cache (`vm_call_method_compiled_cache.rs:176-260`) does not key on the
  boundary; correctness today depends on the interpreter re-entry path bypassing that cache
  (`class_dispatch.rs:144` calls `resolve_method_with_owner_invocant` directly). Any rewrite
  must preserve that invariant.
- `samewith` state is split across two parallel stacks kept in lockstep only by convention:
  `samewith_context_stack: Vec<(String, Option<Value>)>` (`mod.rs:1870-1873`) and
  `samewith_call_args_stack: Vec<Vec<Value>>` (`mod.rs:1874-1881`, added 2026-08-13 for the
  native-array fallback, whose sole reader is `builtins_dispatch_next.rs:296`). Only
  `push_method_samewith_context`/`pop_method_samewith_context` (`accessors_state.rs:736-754`,
  `:947-957`) push both; at least five sites push the context stack RAW with no args entry
  (`methods_mixin_dispatch.rs:212`, `dispatch_proto_call.rs:137`,
  `builtins_operators_fallback.rs:460`, `builtins_dispatch_next.rs:165`, plus the VM sub
  paths `vm_call_dispatch.rs:295` / `vm_call_func_ops.rs:627`, `:1233`) — so
  `samewith_call_args_stack.last()` can pair with the WRONG context whenever a raw push sits
  above a helper push. A latent desync class, not a reproduced bug.

### Design decisions

**1. `{*}` = direct resolution within the governing boundary; the ambient state dies.** A new
parameterized resolver — `resolve_method_within_boundary(receiver_class, method_name, args,
invocant, boundary_owner: Option<Symbol>)` — hoists `resolve_method_with_owner_impl`'s
truncation (`resolution_method.rs:135-144`) into an explicit argument; the ambient
`proto_redispatch_boundary` field (`mod.rs:1182-1197`), its bracket
(`dispatch_proto_call.rs:61-73`), and its init/fork sites are deleted, and
`format_method_candidate_signatures` takes the boundary as a parameter instead of reading
interpreter state (`class.rs:310-319`). The method branch of `call_proto_dispatch` becomes:

1. Owner from `lookup_proto_method` (unchanged — it already names the governing class).
2. rw-args rebuild ("part A") unchanged; it feeds the resolver's `args`.
3. `resolve_method_within_boundary(..., Some(owner))` picks the winner directly — no by-name
   re-entry, so `proto_method_skip` (set `:50`, consumed `dispatch_proto.rs:321-324`) is
   deleted with nothing replacing it: there is no re-entry left to intercept, and a candidate
   body's own fresh calls to the same method correctly re-enter the proto from the top (P3's
   observed `samewith` behavior depends on exactly that).
4. The winner is invoked through the SAME resolved-method run path ordinary dispatch uses
   (the `run_instance_method_celled` → `build_remaining` → run leg,
   `class_dispatch.rs:124-`), so the deferral frame for `nextsame`/`callsame` inside
   candidates is built identically to today. "Part B"'s pending-source restore shrinks to a
   tight set immediately before that invocation (same mechanism, no longer spanning a whole
   re-dispatch pipeline).
5. No-match raises `X::Multi::NoMatch` with the truncated candidate listing produced from the
   same boundary parameter (diagnostic parity with #6355).

Gains, in the repo's terms: one resolution walk instead of two `lookup_proto_method` walks
plus a full by-name re-dispatch; the skip flag (name-keyed only, no invocant identity) and
the body-spanning ambient bracket both retire as hazard classes; the VM-cache invariant
becomes structural (the boundary is a parameter of an interpreter-side resolver that never
touches the cache) instead of an accident of which path bypasses what. The observable
behavior is #6355's — this is a same-answer mechanism swap, gated accordingly.

**2. `samewith` stays by-name; E9c's samewith work is carrier consolidation only.** Per P3,
the restart semantics are correct as implemented. What E9c fixes is the state carrier: merge
the two parallel stacks into one
`Vec<SamewithContext { name: String, invocant: Option<Value>, args: Option<Vec<Value>> }>`
with a single push/pop helper pair; every raw push site goes through the helpers (`args:
None` where no original-args carrier is needed), eliminating the lockstep-by-convention
desync class. `builtin_samewith` and every name-only reader
(`method_name_for_dispatch`, the native fallbacks, the wrap peek) read the same struct.

**3. Untouched, deliberately:** the VM `{*}` arm keeps delegating the method case to the
interpreter (`vm_call_func_ops.rs:1747-1749` — compiling the boundary-resolved dispatch is
follow-on perf work, not semantics); the SUB branch of `call_proto_dispatch` (`:76-150`) and
the sub-side `multi_dispatch_stack` mechanics (Phase C/F); `proto_dispatch_stack` and
`ProtoMethodCtx` themselves (they carry invocant + call-site arg sources from interception to
`{*}` and are the right shape already); the P4 type-object gate (its ticket).

### Slice plan

- **E9c-1** — `SamewithContext` consolidation (mechanical, zero behavior) + the P3 pin
  `t/samewith-proto-body-rerun.t` (green on both engines today — it encodes ground truth so
  the E9c-2 rewrite cannot silently regress the restart semantics).
- **E9c-2** — the `{*}` direct-resolution cutover (decision 1) with its deletion list
  (`proto_method_skip`, `proto_redispatch_boundary` + bracket + fork/init sites, part B's
  broad restore). Same-answer swap, but it rewires a real dispatch path: gate on the proto
  pin corpus (`t/proto-*.t`, `t/multi-*.t` — 61 files as of #6355), the E9-pre pins, local
  `make roast` (house rule for name/type-resolution changes), and the E8-era roast slice plus
  `S06-multi/proto.t`.

E9b and E9c are independent (E9b lives in `builtins_dispatch_next.rs`/frame types; E9c in
`dispatch_proto_call.rs`/`resolution_method.rs`); either order works, but E9b-0 (the P2
ordering fix) is the highest-value single slice across both and should go first.

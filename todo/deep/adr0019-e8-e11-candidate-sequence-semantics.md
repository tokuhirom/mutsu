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

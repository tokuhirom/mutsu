# ADR-0019 E2/E3/E4 design: native handler rows, the one resolver, and the resolved-call cache

**Status: E3 and E4 are closed; E2 alone remains open**, as a non-gating cleanup (see the ADR's
G4 closure note: "E2's exact-handler-ID catalog ... open cleanup, no longer gating dispatch
correctness"). E2b coverage is driven to ~99% with a structural fallback in place, so the
`native_call_unmodeled` counter and the row-completeness work below is the only live remnant of
this doc; treat the E3/E4 sections as the ADR's linked closed-box detail record, and the "no code
has landed for these boxes yet" line below as stale for E3/E4 specifically.

Design pass for Phase E boxes E2 (exact handler IDs), E3 (generation-keyed resolved-call cache),
and E4 (one MRO walk for native + user candidates). These three are one mechanism seen from
three sides — the rows are what the resolver reads, the cache is where its result lives — so
they are designed together and land interleaved (E2a → E4a → E2b → E4b → E3, see the slice
plan). Depends on E1 (`TypeId`, `receiver_dispatch_class`, `dispatch_mro`); see
`adr0019-e1-typeid-receiver-owner.md`. No code has landed for these boxes yet.

## Postmortem: why the 2026-08-04 handler-ID attempt was reverted

Commit b252837e7 added `handler: NativeMethodHandlerId::PureArity` to every
`BuiltinMethodEntry`, cached `(Symbol, Symbol) → Option<NativeMethodHandlerId>` on the
interpreter, and made `vm_native_dispatch` consult the registry row before invoking the
arity-specific implementation. Reverted same day (f1485d136). Two structural causes:

1. **Wrong owner.** The registry probe keyed on `value_type_name()` output: type objects
   probed as `Package`, user Array subclasses as `Any`, `Map`-declared hashes needed the alias
   special case — so the row lookup answered "no entry" (or the wrong entry) for receivers the
   string-match cascades accept. This is E1's job; E2 must not start before E1b.
2. **One-step flip with no shadow phase.** The row catalog (14 name slices, ~350 slots) is a
   *strict subset* of what the match cascades accept (~700 quoted-name arms across
   `methods_0arg/` and `methods_narg/` — e.g. `Failure`, temporal, `Version`, allomorph and
   `Rat`-specific arms have no catalog row at all), and the flip made the incomplete rows
   load-bearing immediately. Coverage has to be *measured to completeness* before any read-side
   behavior depends on the rows.

## Facts the design rests on (survey results, 2026-08-09)

**The native layer:**

- Three pure entries, dispatching by `match` on the resolved method-name `&str`:
  `native_method_0arg(&Value, Symbol)` (`builtins/methods_0arg/mod.rs:304`),
  `native_method_1arg` (`methods_narg/dispatch_1arg.rs:25`), `native_method_2arg`
  (`dispatch_2arg.rs:17`). Return `Option<Result<Value>>`; `None` = not handled. **No 3+-arg
  entry exists** — 3-arg natives are hand-rolled pre-dispatch escapes
  (`vm_native_dispatch.rs:159,345,355,364`). No `_mut` variants exist; mutation is Tier-A VM
  helpers (`try_native_array_mut` etc. in `vm_call_method_mut_ops.rs`) that write back into env
  by name themselves.
- The admission gate `try_native_method_raw` (`vm_native_dispatch.rs:38-492`) runs ~16 checks
  before the arity switch, and `should_bypass_native_fastpath`
  (`runtime/methods_native_bypass.rs:116`) is its independently-maintained interpreter twin.
  Both consult `is_native_method` (`class_introspection.rs:63-221`) — itself a hand-coded
  per-class name-list cascade plus the `ClassDef::native_methods` registry tail.
- `BuiltinMethodEntry { owner, name, order }` (`builtin_type_methods.rs:718-726`) carries **no
  arity, no mutability, no handler, no type-object admissibility**.

**The resolution layer (what E4 unifies):**

- The ranked resolver is `resolve_method_with_owner_impl` (`resolution_method.rs:116-382`):
  MRO walk collecting `(owner, MethodDef)` matches, then a tie-break ladder (type distance →
  narrowness tuple → explicit-named → most-derived owner → `is default` → ambiguous).
- At least six sibling walkers re-implement parts of it: `resolve_all_methods_with_owner`
  (:551, the nextsame list), `resolve_methods_per_mro_level` (:635, `.+`/`.*`),
  `count_visible_method_candidates` (:518), `has_multiple_dispatch_candidates`
  (`accessors_state.rs:538`), `multi_dispatch_type_cacheable`
  (`vm_call_method_compiled_cache.rs:66`), the private-method resolver family
  (`resolution_private_method.rs`), plus `resolve_user_method_or_accessor`
  (`class_introspection.rs:280`) and WALK's own (`methods_walk.rs:583`).
- The submethod no-inherit rule (`def.is_my && is_ancestor → skip`) is copy-pasted at six
  sites (`resolution_method.rs:179,161,541,586,650`, `accessors_state.rs:568`,
  `class_dispatch.rs:124`).
- Native candidates are **not** part of any resolution result today: after user resolution
  fails, control falls through hand-ordered probe sequences per entry point, and `nextsame`
  reaching a native base needs four synthesized fallbacks
  (`native_{grammar_parse,mu_base,array_storage,metamodel}_next_candidate` in
  `builtins_dispatch_next.rs`).

**The cache layer (what E3 replaces):**

- `MethodEntry { builtin, user_candidates: Vec<MethodDef>, accessor }` keyed
  `(owner: Symbol, name: Symbol)` with a monotonic `method_generation` bumped at exactly 4
  sites inside `registry.rs` (:293, :321, :349, :415). One read-boundary refresh,
  `refresh_method_caches_for_generation` (`vm_call_method_compiled_cache.rs:4-18`), clears 7
  caches wholesale on generation change; it is called from only 2 places, and several probe
  sites (`native_ctor_plan`, `has_multiple_dispatch_candidates`, private-method resolve,
  accessor reads) bypass it and rely on the ~24 manual clear blocks (88 statements across 13
  files).
- No cache key encodes call shape. `fast_method_cache` is keyed `(class, method)` with arity
  as a post-hit *guard*; named-arg calls simply decline the multi cache
  (`multi_arg_type_keys` returns `None` for Pair/Instance/… at
  `vm_call_method_compiled_cache.rs:37-51`). The whole fast cache is disabled globally
  whenever any wrap chain exists (`has_any_wrap_chains`, `accessors_state.rs:842`).
- `fn_resolve_gen` is a second, function-side generation scheme (bumped ~30 sites); wrap
  mutation bumps only it, never `method_generation`.

## Design decisions

**1. E2 rows are *recognition metadata*, not function pointers — invocation stays in the
arity cascades until F3.** The row schema:

```rust
pub(crate) struct NativeMethodRow {
    pub owner: &'static str,          // canonical dispatch owner (E1 TypeId name)
    pub name: &'static str,
    pub order: u16,                   // existing .^methods catalog order
    pub arity: NativeArityMask,       // bitmask: A0 | A1 | A2 | N (slow-path/special)
    pub flags: NativeRowFlags,        // see below
}
bitflags NativeRowFlags {
    TYPE_OBJECT_OK,     // callable on a type object (e.g. .Str on Str:U is "" + warn, .raku, .gist)
    MUTATES_RECEIVER,   // implemented by a Tier-A mut helper / mut slow path, not the pure layer
    SPECIAL,            // handled by a named interceptor, never by native_method_*arg
}
```

`BuiltinMethodEntry` grows into (or is replaced by) this row; `MethodEntry.builtin` carries it
unchanged. What the resolver ultimately needs from a row is the answer to "may this
(owner, name, shape, definedness) be served natively, and by which tier" — that is recognition,
not invocation. Converting ~700 match arms into per-method function pointers is exactly F3's
retirement of the hand tables and is *not* required for any Phase E box; sequencing it here
would repeat the reverted attempt's mistake at 20× the diff.

**2. Row completeness is driven by counters to a measured zero, before any read depends on
rows.** Two instruments:

- Runtime shadow counter `native_call_unmodeled` (`MUTSU_VM_STATS`-gated): bumped whenever
  `native_method_{0,1,2}arg` returns `Some(..)` for an `(owner, name)` that has no row (or a
  row whose arity mask excludes the call's arity). Swept over full `t/` plus whitelisted roast;
  every hit is a missing/wrong row to add.
- Test-side inverse probe (cfg(test), the B1/B2 precedent allows probing in tests): for each
  row with arity bit A0/A1/A2 and not SPECIAL/MUTATES, call the corresponding cascade with a
  representative receiver of the owner type and assert recognition. This catches rows that
  claim coverage the cascades do not provide (the reverted attempt's silent wrong-entry mode).

The pinned regression cases the ADR names (each becomes a `t/` test in E2a, before anything
flips): a type object receiving a catalog method (`Str.gist`, `Int.raku`), a user `is Array`
subclass calling `push`/`elems` (storage delegation), `Map` receiving Hash-owned methods and
its own, a gather `Seq` vs `Array` LazyList, `Failure` (must explode on unhandled use, not
dispatch — `methods_call_dispatch.rs:629` — so Failure rows must be SPECIAL), `<1/3>` Rat and
`FatRat` methods, allomorph (`IntStr`) dispatch.

**3. The admission gate splits into method-identity facts (→ rows) and receiver-state facts
(→ resolver guards).** Classification of `try_native_method_raw`'s checks:

| Check (vm_native_dispatch.rs) | Class | Destination |
|---|---|---|
| deferred-Seq bail :81, lazy-pipe :116, lazy guard :135 | receiver state | resolver guard (pre-probe) |
| NativeCall `.REPR`/`.WHERE` :94, `.Capture` :103 | method identity | SPECIAL rows |
| Proxy receivers :141 | receiver state | resolver guard |
| `squish`/`tail` always-interpreter :150 | method identity | row absent / SPECIAL |
| 3-arg `contains`/`starts-with`/`substr-eq`/buf-write :159,345,355,364 | method identity | rows with arity N (slow-path) |
| `mixin_role_has_method` :165 | receiver state | resolver (mixin layer precedes native in the chain) |
| Instance/lazy-Match arms incl. `is_native_method` :171-262 | both | user candidates + `native_methods` registry rows |
| Supply lists :192,266 | method identity | rows under Supply owner |
| collection-`.gist`-with-Instance :294, `container_needs_raku_dispatch` :301 | receiver state | resolver guard |
| post-switch fixups (:381-491 decode/Map-gist/clone-retag) | execution detail | stay with invocation |

`should_bypass_native_fastpath` (`methods_native_bypass.rs`) is the same list re-implemented;
E4b's cutover deletes it in favor of the one resolver-side guard set. This table is the E2b
work list.

**4. E4: one resolver producing one canonical result type.**

```rust
pub(crate) struct ResolvedSequence {
    pub generation: u64,                       // registry method_generation at build time
    pub candidates: Arc<[ResolvedCandidate]>,  // full ordered sequence, most-derived first
    pub proto: Option<ProtoRef>,               // proto method interception (E8)
}
pub(crate) enum ResolvedCandidate {
    User { owner: TypeId, def: Arc<MethodDef> },              // stored order within a level
    Accessor { owner: TypeId, public: bool },                 // generated attribute accessor
    Native { owner: TypeId, row: &'static NativeMethodRow },  // recognition; execution per tier
}
```

`resolve_sequence(chain: &[TypeId], name: Symbol, shape: CallShape, definedness) ->
Option<ResolvedSequence>` walks the E1 chain once. Per level: user candidates from
`MethodEntry.user_candidates` in stored order (skipping `is_private`; skipping
`is_my` candidates when level > 0 — the six copy-pasted submethod rules collapse into this one
line); the accessor bit from `MethodEntry.accessor` with today's
`resolve_user_method_or_accessor` arbitration; the native row if the level is a catalog owner.
**Signature matching and ranking stay per-call**: the sequence is the shape-independent
candidate universe; the existing ranker (`resolve_method_with_owner_impl`'s ladder, extracted
to take a candidate slice instead of doing its own walk) selects the winner from it per call.
This is what lets E9 later give `nextsame` a cursor and lets the native base be "just the next
candidate" — deleting the four synthesized native fallbacks.

E4 lands shadow-first like E1: **E4a** builds the sequence beside the current resolution at the
two `resolve_method_cached` boundaries and compares the winner
(`owner symbol + MethodDef body fingerprint`) under counters `resolver_shadow_checks` /
`resolver_shadow_mismatches`, user candidates only. **E4b** adds native rows to the sequence,
makes the sequence authoritative at those boundaries, and deletes
`should_bypass_native_fastpath` in favor of the unified guard set. The sibling walkers
(`resolve_all_methods_with_owner` etc.) are *not* rewritten in E4 — they migrate with their
consumers (E8/E9 for the deferral list, E7 for WALK/lookup).

**5. E3: cache the sequence, keep the wholesale-clear generation discipline.**

```rust
resolved_seq_cache: FxHashMap<(TypeId, Symbol, CallShape), Arc<ResolvedSequence>>
// CallShape: packed { arity_bucket: 0|1|2|3+, has_named: bool }
```

- The cache joins `refresh_method_caches_for_generation`'s clear set; the per-entry
  `generation` stamp is a debug assertion, not the invalidation mechanism (wholesale clear on
  generation change is today's proven model and avoids stale-entry resurrection reasoning).
- `CallShape.has_named` exists so named-arg calls get *sequence* caching (today they decline
  the multi cache entirely); the named-aware winner selection still runs per call.
- `fast_method_cache` is **not** deleted in E3: it survives as the monomorphic in-line layer in
  front (its post-hit guards are load-bearing: arity/defaults/attr-alias/container-sharing,
  `vm_call_method_compiled_interpret.rs:493-539`). F5 retires it once the sequence cache plus
  E10's generation-covered wraps make it redundant — retiring it early would put an unmeasured
  perf cliff inside Phase E.
- The read boundary gains the two missing refresh calls (private-method resolve and
  `has_multiple_dispatch_candidates` probe sites) so the generation actually covers every
  method-shaped cache read; the manual clear blocks stay until F5 (they are then provably
  redundant instead of speculatively deleted).

**6. Perf gate.** The resolver adds work only on fast-cache misses (the IC stays in front), but
E4b/E3 must show bench-CI parity (fib, bench-tak, and one dispatch-heavy bench) before their
boxes close — G3's "cache-hit dispatch remains generation-checked O(1)" is checked here, not
deferred to the end of the migration.

## Slice plan

- **E2a — row schema + instruments + pinned tests.** `NativeMethodRow`/flags, rows generated
  from the existing 14 name slices (arity/flags initially conservative: arity mask from which
  cascade file names the method, SPECIAL where §3's table says so), the `native_call_unmodeled`
  counter, the cfg(test) inverse probe, and the regression tests from decision 2. Zero behavior
  change.
- **E4a — sequence builder + shadow parity (user candidates only).** Counter-verified against
  `resolve_method_with_owner_impl` outcomes on full `t/` + roast S12/S14/S32 sweeps. Zero
  behavior change.
- **E2b — drive `native_call_unmodeled` to zero.** Add missing rows file-by-file through the
  gate-classification table; each sub-PR is mechanical row addition plus its sweep evidence.
- **E4b — authoritative switch at the cached-resolve boundaries + native rows in sequence +
  delete `should_bypass_native_fastpath`.** Local `make roast` before PR (semantics-touching).
- **E3 — the sequence cache + refresh-coverage completion.** Bench evidence in the PR.

Expected to subdivide further from the E2b sweep the way C6 did; the boxes above are the
ordered layers, not necessarily five single PRs.

## Verification items

- **V1**: enumerate every quoted-name arm in `methods_0arg/`/`methods_narg/` (scripted count,
  committed as the E2b checklist) and reconcile against rows; the ADR's "~700 arms vs ~350
  slots" gap must end at zero unmodeled hits, with SPECIAL/absent rows justified per arm.
- **V2**: confirm `MethodEntry.user_candidates` order equals `ClassDef::methods` stored order
  under role composition and `also does` (it is synced verbatim, `registry.rs:326-334`, but
  `resolve_class_stub_requirements` mutates order at `registration.rs:347-369` — the sequence
  must be built *after* those mutations, which sync timing already guarantees; add a test).
- **V3**: definedness filtering — confirm where `:D`/`:U` invocant constraints are enforced
  today (bind time) and keep them there; the sequence carries definedness only for E4's
  candidate admission of type-object calls (TYPE_OBJECT_OK rows).
- **V4**: thread-fork behavior — the new cache resets in `clone_for_thread_excluding` with the
  other method caches (`runtime_thread.rs:704-714` block).

## Risk notes

The reverted attempt is the risk model: rows becoming load-bearing while incomplete. The
counter-to-zero discipline (E2b) and shadow-first sequencing (E4a before E4b) are the
mitigations. **Gate renegotiated 2026-08-10** (see the ADR's E2b twelfth-slice note): after
twelve E2b slices `native_call_unmodeled` is down ~99% (~37904 to ~400) with no dominant
cluster left in the diminishing-returns tail, so a literal-zero precondition is replaced by a
structural one — E4b's resolver falls back to the pure native-arity cascade on any row miss
(rather than treating a miss as "no candidate"), so an incomplete table degrades to today's
behavior instead of misdispatching, and `native_call_unmodeled` continues to fire through that
fallback path in production as an ongoing monitoring signal. `resolver_shadow_mismatches`
already carries the same spirit of precedent from E4a's own landing (3 mismatches / 0.012%,
one explained, bucketed shape — not literally zero either): a *new, unexplained* mismatch shape
blocks a box; an explained, ledgered one does not. Second risk: perf regression from double
resolution during shadow phases — shadow probes are `MUTSU_VM_STATS`-gated so the default build
pays one branch, not two resolutions.

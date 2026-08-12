# `method_entries` (the E1/E2 canonical method table) never covers an un-punned role owner

## Root cause

`Registry::sync_user_method_entries(class_name)` (`src/runtime/registry.rs`) is the only
writer of `Registry::method_entries` for user-declared methods (the `MethodEntryKey { owner,
name } -> MethodEntry { user_candidates, .. }` table introduced by ADR-0019's E1/E2 boxes).
Its body reads `self.classes.get(class_name)` and returns early (after clearing any stale
entries for that owner) when the lookup fails:

```rust
let Some(class_def) = self.classes.get(class_name) else {
    self.bump_method_generation();
    return;
};
```

A **role** is never a key in `self.classes` unless it has been *punned* (used as a standalone
type via `RoleName.new`, which briefly registers a synthetic `ClassDef` for the pun and calls
`sync_user_method_entries` on it — see `methods_object_dispatch_new.rs`'s `withdraw_role_pun`,
which also calls `sync_user_method_entries` again on withdrawal, clearing the entries right
back out). So for a role that is never punned during a run — the overwhelmingly common case,
since roles are normally only composed into a class, not instantiated directly —
`Registry::method_entries` has NO entry at all for that role's own name, at any point.

Consequently `Registry::user_method_overloads(role_name, method_name)` (and its alias
`get_method_overloads`) always return `None` for such a role, even though the role's own
methods are fully present in `Registry::roles[role_name].methods` (the older, still-live raw
storage) the whole time.

## Where this actually bites today

`resolve_all_methods_with_owner` (`resolution_method.rs`, the "remaining"/deferral-list walker
`push_method_dispatch_frame` uses for `nextsame`/`callsame`) does **not** go through
`get_method_overloads` — it reads `self.registry().classes.get(cn)... .or_else(|| ...roles.get(cn)...)`
directly, so it sees a role's raw methods regardless of punning. This is why the REAL
`nextsame`/`callsame` deferral list has always correctly included an un-flattened role method
(e.g. a role's stub `method process(...) { ... }` that a composing class overrides with its
own `method process`, `t/supply-nested-whenever-emitter.t`; two conflicting same-named role
methods left un-flattened when the class itself supplies the resolving override,
`t/role-conflict.t`; a role-qualified call `self.R::me()`, `t/qualified-method-call.t`).

But **`resolve_method_with_owner_impl`'s own per-level walk DOES go through
`get_method_overloads`** (`resolution_method.rs:140`) — the production WINNER-selection path.
Today this gap is *masked* for winner selection in the ordinary case: a composed role's method
is normally flattened onto the class itself (a `ClassDef.methods` copy, tagged
`role_origin`), so the walk's early-stopping return (the first MRO level with `Some(overloads)`
at all — usually the class's own level, which precedes any role in the MRO) fires before the
walk ever reaches the role's own un-punned MRO level. The gap would only surface for winner
selection if a role-owned candidate were the FIRST usable candidate reached in an MRO walk with
no preceding class-level entry for that name at all — not observed as a live production bug,
but not proven absent either; it has simply never been exercised by the existing `t/`/roast
corpus in a way that surfaces as a wrong *answer* (as opposed to `resolve_sequence`'s new
shadow-check, which walks the FULL sequence without early-stopping and so sees the gap on the
DEFERRAL side directly).

`ctor_phase_plan.rs:133`, `vm_call_method_compiled_cache.rs:97`, and all three call sites in
`resolution_private_method.rs` also read through `get_method_overloads`, so all of them share
this same latent role-coverage gap.

## Why this was found now

ADR-0019 Phase E box E8a (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`,
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`) added `Interpreter::resolve_sequence`
-based shadow-checking of the `nextsame`/`callsame` deferral list
(`Interpreter::shadow_check_deferral_sequence`, `src/runtime/resolution_sequence.rs`).
`resolve_sequence` builds its candidate universe via `Registry::user_method_overloads` (the
E1/E2 canonical table), so it inherits this exact gap: it silently omits any un-punned role's
own raw candidates. A `MUTSU_VM_STATS=1` sweep of `t/` (2026-08-12) found every deferral-list
shadow mismatch traced to this one root cause (`real_len` one candidate ahead of `shadow_len`,
the missing candidate always owned by an un-punned role reachable in the receiver's MRO) —
see the E8a landing note in the ADR and in
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` for the exact file list and counts.

## Why this is not fixed inside E8a

Extending `sync_user_method_entries` to also populate role owners (the obvious fix — mirror the
class branch, reading `self.roles.get(class_name)` when `self.classes.get(class_name)` misses)
is not a "shadow-only, zero-real-behavior-change" edit: `get_method_overloads` is read by
**real production dispatch** at the sites listed above, including winner selection
(`resolve_method_with_owner_impl`) and private-method resolution (all three
`resolution_private_method.rs` sites). Populating previously-empty role entries could change a
real dispatch OUTCOME for some MRO shape not covered by today's tests (most likely a genuine
bugfix, but an unverified one), which is out of scope for a box whose own definition
(`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`, E8a slice) is limited to adding
sequence structure and shadow-comparing it — not changing what any real call resolves to. E8a
therefore left this as a **documented, accepted shadow-check divergence** (mirroring E4a's own
accepted-divergence bucket for the winner probe) rather than fixing it inline.

## Update 2026-08-12: E9-pre ground truth complicates "real walker is authoritative"

The ADR-0019 E9-pre raku verification campaign found that for the class-overridden shape (a
`does`-composed role method that the composing class overrides with its own same-name method),
**raku EXCLUDES the role's method from the nextsame/callsame chain** — i.e. the sequence side's
omission matches raku there and the real walker's inclusion is the bug
(`todo/tickets/role-shadowed-method-in-defer-chain.md`). Do not resolve this ticket by simply
populating role entries so the sequence matches the real walker: the E8a mismatch ledger must be
re-audited per-shape against raku first (qualified `self.R::m()` calls and conflict-resolution
shapes may still legitimately need the role's own entry; the shadowed-by-class shape must not).

## Update 2026-08-13: the shadowed-by-class ticket is fixed; this ticket's own scope is unchanged

`todo/tickets/role-shadowed-method-in-defer-chain.md` is resolved
(`news/2026-08/role-shadowed-method-in-defer-chain.md`) — `drop_flattened_role_duplicates`
(`resolution_method.rs`) now excludes a `does`-composed role's raw entry from the real walker's
output whenever a class-owned method of matching signature shadows it, so the real walker no
longer disagrees with raku for that shape. This ticket's own subject — `method_entries` never
covering an un-punned role at all, affecting the four *production dispatch* call sites listed
above — is untouched by that fix and remains open exactly as scoped.

## Suggested fix, for whoever picks this up

- Add a role branch to `sync_user_method_entries` mirroring the class branch (`self.roles.get(class_name)`
  when `self.classes.get(class_name)` is `None`), and call it from wherever a role finishes composing
  (parallel to the class-body-exit call sites) so a role's `method_entries` are populated the moment
  it exists, without depending on punning at all.
- Before landing, raku-verify (per this repo's "measure before assuming" house rule) that no MRO shape
  changes its resolved winner because of the newly-visible role-level entries — the concern above is
  theoretical, not a confirmed bug, but the four production call sites this feeds need a dedicated pass,
  not a drive-by fix bundled into an unrelated box.
- Once fixed, re-run E8a's `MUTSU_VM_STATS=1` sweep over `t/` + the relevant roast slice
  (`roast/S06-advanced/*`, `roast/S12-methods/*`, `roast/S14-roles/*`) — the deferral-shadow mismatch
  count should drop to (or very near) zero, and the explained-divergence note in
  `src/runtime/resolution_sequence.rs`'s module doc / the ADR's E8a progress note can be removed.

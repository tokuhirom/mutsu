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
`news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md`) added `Interpreter::resolve_sequence`
-based shadow-checking of the `nextsame`/`callsame` deferral list
(`Interpreter::shadow_check_deferral_sequence`, `src/runtime/resolution_sequence.rs`).
`resolve_sequence` builds its candidate universe via `Registry::user_method_overloads` (the
E1/E2 canonical table), so it inherits this exact gap: it silently omits any un-punned role's
own raw candidates. A `MUTSU_VM_STATS=1` sweep of `t/` (2026-08-12) found every deferral-list
shadow mismatch traced to this one root cause (`real_len` one candidate ahead of `shadow_len`,
the missing candidate always owned by an un-punned role reachable in the receiver's MRO) —
see the E8a landing note in the ADR and in
`news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md` for the exact file list and counts.

## Why this is not fixed inside E8a

Extending `sync_user_method_entries` to also populate role owners (the obvious fix — mirror the
class branch, reading `self.roles.get(class_name)` when `self.classes.get(class_name)` misses)
is not a "shadow-only, zero-real-behavior-change" edit: `get_method_overloads` is read by
**real production dispatch** at the sites listed above, including winner selection
(`resolve_method_with_owner_impl`) and private-method resolution (all three
`resolution_private_method.rs` sites). Populating previously-empty role entries could change a
real dispatch OUTCOME for some MRO shape not covered by today's tests (most likely a genuine
bugfix, but an unverified one), which is out of scope for a box whose own definition
(`news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md`, E8a slice) is limited to adding
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

## Update 2026-08-15: the "obvious fix" causes a real regression, confirmed by repro

Tried the suggested fix exactly as written (add a role branch to `sync_user_method_entries`
mirroring the class branch, call it from `finish_role_registration` right after
`self.registry_mut().roles.insert(name.to_string(), role_def)`). It builds clean and passes the vast
majority of `t/`, but breaks `t/multi-method-roles.t` deterministically — confirmed with a minimal
repro (no Test module):

```raku
role R5 {
    multi method rt()       { say 'empty' }
    multi method rt(Str $a) { say 'Str'   }
}
role R6 {
    multi method rt(Numeric $a) { say 'Numeric' }
}
class C { has @.order }
my C $b1 .= new();
$b1 does (R5, R6);
$b1.*rt;
```

raku prints `empty`; with the fix applied mutsu throws `No matching candidates for method: rt`.
`t/role-required-universal-method.t` and `t/mixin-compiled-attr-writeback.t` also broke (not yet
root-caused individually, but very likely the same mechanism).

**Root cause traced with `rust-gdb -batch` (breakpoint on `make_multi_no_match_error`, `bt`):**
`does (R5, R6)` on a real `Instance` goes through `does_rebless_instance`
(`role_mixin_class.rs`), which reblesses `$b1` into a real composed class `C+{R5,R6}` (via
`ensure_mixin_class` → `register_class_decl`, which flattens both roles' `rt` candidates onto the
new class's own `ClassDef.methods`) — this is unrelated to and unaffected by this ticket's fix.
The break is in `resolve_methods_per_mro_level` (`resolution_method.rs:742`), the winner-selection
path `call_method_all_with_values` uses for `.*`/`.+`:

1. It walks `class_mro("C+{R5,R6}")`, which includes `R5` and `R6` themselves as MRO members (for
   qualified-call support), and collects every level where `get_method_overloads(level, "rt")` is
   `Some` into `defining_levels`. Before this fix that was only the class's own flattened level;
   after this fix `R5` and `R6` are *also* `Some` now (their own raw candidates), so
   `defining_levels = ["C+{R5,R6}", "R5", "R6"]`.
2. Because `rt` is multi, it then calls `resolve_method_with_owner_impl(cn, "rt", args, None, None)`
   **once per defining level, with that level's own name as the MRO-walk start** — i.e. it
   re-resolves `rt` starting fresh from `"R5"` and separately from `"R6"`, not just from the
   receiver's own class.
3. `$b1.*rt` (zero args) matches `R5`'s `rt()` and the class's own flattened `rt()`, but **`R6` only
   has `rt(Numeric $a)`, which does not match zero args** — `resolve_method_with_owner_impl("R6",
   ...)` returns `None`, setting `any_failed = true`.
4. `any_failed` makes the whole function return `Vec::new()` (see the `if any_failed { return
   Vec::new(); }` a few lines below the per-level loop) — every candidate is discarded, including
   the ones that DID match — and the caller then raises `X::Multi::NotFound` believing genuinely no
   candidate matched.

This is a real, general hazard, not a corner case: **any composed-role multi method where one
role's own candidate set doesn't cover a given call's arguments will now spuriously fail `.*`/`.+`
dispatch**, even though the flattened class-level candidate set (which already worked before) is
unaffected. The "any_failed ⇒ discard everything" logic in `resolve_methods_per_mro_level` was
written assuming every `defining_levels` entry is a class the receiver actually descends from with
its own complete candidate set — it silently assumed no MRO level could be a role whose candidates
are also (partially or fully) duplicated by a more-derived flattened level, which is exactly what
`does_rebless_instance`'s composition produces.

**Why this was not just "restrict the role branch to the 4 originally-named production call sites"
in the first place:** `get_method_overloads`/`user_method_overloads` is the ONE function all
production readers (winner selection included) already share — this ticket's original framing (the
gap "bites" 4 specific call sites) undercounted the exposure, because *any* caller reachable through
that same read function inherits the newly-visible role rows, including
`resolve_method_with_owner_impl`'s own per-level walk inside `resolve_methods_per_mro_level`, which
was not on the original list of 4 "bites here" sites but turned out to be the one that broke.

**What a real fix needs, not attempted here:** either (a) give `resolve_methods_per_mro_level` (and
anywhere else with the same "one failing level discards everything" shape) a way to recognize a
role-owned defining level whose candidates are a subset of a more-derived flattened level and skip
re-resolving it from scratch, rather than adding a second, disjoint read path; or (b) don't populate
`method_entries` for role owners through the shared `sync_user_method_entries`/
`get_method_overloads` machinery at all — instead give the 4 originally-named call sites
(`resolve_all_methods_with_owner` already does this) their own direct `self.roles.get(...)` fallback
that winner-selection code never touches. (b) is more surgical and matches the "one consumer family
per sub-PR" precedent the rest of ADR-0019 Phase E used, but is more code (four separate small
patches instead of one shared one) and still needs the same raku-verification-per-shape discipline
this ticket already called for. Whoever picks this up next should read
`resolve_methods_per_mro_level`'s full body first — the any_failed/all-or-nothing gate is the actual
landmine, independent of which population strategy is chosen.

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

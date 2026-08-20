# The un-punned-role gap in `method_entries` is closed — the asymmetry is the design now

`todo/deep/method-entries-never-covers-unpunned-roles.md` was opened on 2026-08-12 out of the
ADR-0019 Phase E8a shadow-check sweep: `Registry::method_entries` — the E1/E2 canonical
`MethodEntryKey { owner, name } -> MethodEntry` table — has no row at all for a **role** that is
never `.new`-punned, because the table's only writer at the time (`Registry::
sync_user_method_entries`) derived every row from `Registry::classes`, and a role is not a
`classes` key unless a pun briefly registered a synthetic `ClassDef` for it. So
`Registry::user_method_overloads(role_name, name)` (and its alias `get_method_overloads`) always
returned `None` for such a role even though `Registry::roles[role_name].methods` held its methods
the whole time. The ticket named four production readers that consulted the table and therefore
inherited the gap, and worried — explicitly as an unproven concern, not an observed bug — that
winner selection could resolve the wrong method for some MRO shape.

Re-verified against `main` on 2026-08-20. **The finding is resolved, and it is resolved as a
decision rather than as a patch**: the question the ticket really raised ("what is the read policy
for a role owner in the canonical method table?") is now answered in ADR-0019 itself, and the
class/role asymmetry that looked like a hole is the deliberate shape of the design.

## What actually happened to it

**The mechanism the ticket described no longer exists.** `sync_user_method_entries` was deleted by
ADR-0019 F4c-9b. `method_entries` is now maintained by per-declaration mutators in
`src/runtime/registry_method_table.rs` (`set_user_methods`, `push_user_method`,
`retain_user_methods`, `clear_user_methods_for_owner`, ...), each of which also maintains the
`owner_method_names` reverse index. The *fact* the ticket reported still holds — a role that is
never punned owns no row — but it is no longer an artifact of one function's early return; it is
stated policy. `registry_method_table.rs`'s own doc comment on `owner_method_names` puts it
plainly: "`RoleDef::methods` is explicitly out of scope for this index".

**ADR-0019 F4a decided the read policy and shipped it** (the box is checked off). Role method
definitions are *composition inputs*, not dispatch entries: the dispatchable form of a role method
is always the flattened copy on the composing class. Two helpers implement that policy —
`Registry::role_method_overloads` (reads `Registry::roles` directly) and
`Registry::get_method_overloads_with_role_fallback` (`get_method_overloads(...).or_else(||
role_method_overloads(...))`) — and only confirmed-safe, non-winner-selection consumers were
migrated onto the second one, one family per sub-PR, each raku-verified:

- all three `resolution_private_method.rs` sites (a corpus probe over 3173 local `t/` files and
  1436 whitelisted roast files recorded 41 opportunities and **zero** hits — a documented no-op
  over the corpus, a gap-closer beyond it);
- `vm_call_method_compiled_cache.rs`'s `multi_dispatch_type_cacheable` gate (a genuinely non-zero
  hit rate — 162 + 102 opportunities — but the walk only accumulates, so the fallback can only
  make the cacheability gate more conservative, never change a resolved value);
- `methods_classhow_dispatch.rs`'s `^add_method` multi-family cloning helper, which turned out to
  be a real raku-confirmed bug (`C.^add_method('n', R.^find_method('m'))` on a never-composed role
  lost every multi candidate but the carrier's own), pinned by
  `t/add-method-alias-unpunned-role-multi.t`;
- `resolution_deferral.rs`'s `own_overloads_at_level`, the per-level lookup of the E9a deferral
  expansion that replaced `resolve_all_methods_with_owner` as the `nextsame`/`callsame` ordering
  source.

`ctor_phase_plan.rs:133`, one of the ticket's original four, needs no change: its only caller
already filters to a real class, so it is structurally unreachable with a role receiver. And
winner selection (`resolve_method_with_owner_impl`, `resolve_methods_per_mro_level`) is
*prohibited* from consulting the fallback — F4a's own rule, restated in the
`role_method_overloads` doc comment.

**The landmine the ticket found while trying the naive fix is fixed too.** The 2026-08-15 attempt
(mirror the class branch, populate role owners through the shared write path) regressed composed-
role multi-method `.*`/`.+` dispatch and was reverted in PR #6478; the root cause was
`resolve_methods_per_mro_level`'s all-or-nothing gate, where one failing MRO level discarded every
other level's successful match. That gate has since been rewritten to return `(matches,
any_failed)` so the caller invokes every match in order before reporting a dispatch error, exactly
as Rakudo does (`news/2026-08/mro-level-any-failed-partial-match.md`). The
`t/multi-method-roles.t` repro the ticket recorded (`role R5` with `rt()`/`rt(Str)`, `role R6` with
`rt(Numeric)`, `$b1 does (R5, R6); $b1.*rt`) now prints `empty` under both raku and mutsu.

**ADR-0019 F4c then closed the storage question as well**, in its design-note section (1): F4c
deletes `ClassDef::methods` only; `RoleDef::methods` stays where it is, and a role-side sibling
table (`Registry::role_method_entries`) is *rejected*, not deferred — a punned role's tagged
`ClassDef` methods and `roles[R].methods`'s untagged originals are genuinely different data live
under the same owner string at the same instant, `RoleDef::methods` is derived from nothing and
nothing is derived from it (so there is no drift for F4's thesis to eliminate), and the resulting
asymmetry "is the point, not a defect: it makes it structurally impossible to feed a role-owned row
to a dispatcher expecting a class-owned one".

## Verification

Production behaviour was re-checked shape by shape against `raku` (all identical on both): a role
method overridden by the composing class with `nextsame`; a role-qualified `self.R::m()` call; a
two-role conflict resolved by the class's own method; a parameterised `does Par[Int]`; a
`does`-composed role whose method the class does not override, plus `.^lookup`/`.can` on it; a
diamond `role D2 does D1` with a `D1::`-qualified call; a role used as a *parent* (`class Foo is
R1`) with `callsame` from the child; and an anonymous `class :: does Searchable` with `callsame`.
The 2026-08-15 `.*`-dispatch repro passes as well. No divergence remains on any of them.

## What is left, and where it went

One residual, and it is not a dispatch bug: the E8a shadow instrument still builds its comparison
sequence with the role-blind `Registry::user_method_overloads`, so it systematically reports
mismatches against the now-role-aware real walker. A `MUTSU_VM_STATS=1` sweep of `t/` still shows
the signature shape (`real_len=1 shadow_len=0`, the missing candidate always owned by an un-punned
role) in role-heavy files such as `t/callsame-punned-role-and-hyper-infix-sub.t`,
`t/anon-class-does-imported-role.t`, and `t/cro-http-battery.t`. That is a small, well-scoped
instrument fix with no design question attached, filed as
`todo/tickets/e8a-deferral-shadow-sequence-is-role-blind.md`; the stale module doc in
`src/runtime/resolution_sequence.rs` (which still describes `sync_user_method_entries` and names
`resolve_all_methods_with_owner` as the real walker) goes with it.

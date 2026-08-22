# A forward-declared role stub (`role R {...}`) used by another role is never upgraded to its real body

Discovered via the doc-diff harness on `raku-doc/doc/Language/typesystem.rakudoc` (around line
611).

## Repro

```
role R2 {...};
role R1 does R2 {};
role R2 {};
class C does R1 {};
say [C ~~ R1, C ~~ R2];
```

- raku: `[True True]`
- mutsu: errors with "No matching candidate found for the parametric role"

## Relationship to the existing deferred "Forward-declaration stub upgrade" cluster

`docs/doc-diff-backlog.md`'s Deferred section already tracks a **"Forward-declaration stub
upgrade"** cluster, but it is scoped explicitly to `sub` stubs (`sub a() {...}; say a; sub a()
{42}`). This finding is the same forward-declare/stub-upgrade idea applied to `role` declarations
instead of `sub` — a distinct code path (role registration vs. sub registration), so it's filed
as its own ticket rather than folded into that sub-scoped cluster. The two probably share a
common underlying design question (how mutsu's hoist pass handles a `{...}` yada-stub being
superseded by a later real definition) and may be worth solving together once someone picks
either one up.

## Root cause guess

`role R2 {...}` registers a stub role; `role R1 does R2 {}` composes against that stub; the later
`role R2 {}` real definition presumably doesn't retroactively update `R1`'s already-composed
role-list/MRO, so when `C does R1` is later checked against `R2`, it's checking against the
stale stub rather than the real (empty but valid) `R2`.

## Affected files (starting point)

- `src/runtime/class.rs` / `src/runtime/registration_role.rs` (or equivalent) — role stub
  registration and later upgrade
- Whatever emits "No matching candidate found for the parametric role" — grep for that exact
  message to find the composition-time role-resolution code

## Suggested next step

Check how `sub`'s stub-upgrade fix (once implemented, per the existing deferred cluster) handles
the hoist-pass + inline-pass double-registration problem, and see whether the same design applies
to `role`'s registration path.

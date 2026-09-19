# bench-array's 16% instruction regression traced to type_matches_value

`bench-array`'s deterministic instruction count (`scripts/bench-det.sh`, tracked in
`bench-det-history.tsv` on the `bench-data` branch) jumped from 1,895,965,883 to
2,203,133,220 `Ir` (+16.2%) around 2026-09-19. The jump was first suspected to be
in the Number::Denominate compatibility fix (the commit adjacent in the recorded
bench history), but `bench-det.sh` samples merge commits rather than every commit,
so the two recorded rows spanned seven real commits.

Bisecting with `valgrind --tool=callgrind` across each individual commit (building
and measuring in isolated `git worktree`s to avoid incremental-cache and stale-binary
confusion) pinned the regression to `29378ef9` ("fix: make Data::StaticTable pass
under mutsu"), specifically a `type_matches_value` change that resolves a nested
type's package-scoped short name (`Position` for `Data::StaticTable::Position`)
before the fast-accept checks:

```rust
let resolved_constraint = self.resolved_type_capture_name(constraint);
if resolved_constraint != constraint {
    return self.type_matches_value(&resolved_constraint, value);
}
```

`resolved_type_capture_name` always returns an owned `String`, so this made *every*
type check in the interpreter allocate, compare, and immediately drop a `String` --
even for the overwhelmingly common case of a plain constraint like `Any` or `Int`
that needs no resolution at all. callgrind attributed ~118M of the ~307M regression
directly to that function, with the rest showing up as extra malloc/free traffic in
`bind_function_args_values_inner`, the positional-parameter-binding hot path every
`push`/`map`/`grep`/`sort` closure call in `bench-array` goes through.

Fixed in two steps:

1. Split `resolved_type_capture_name` into a `String`-returning wrapper (unchanged
   public behavior, still used everywhere else) and an `Option<String>`-returning
   core (`try_resolved_type_capture_name`) that `type_matches_value` calls directly,
   so the no-resolution-needed case costs zero allocations instead of an
   allocate-compare-drop cycle.
2. Added a cheap disjunction in `type_matches_value` that mirrors the core's own
   early-outs (a type capture ever bound in this process, a package alias ever
   registered, or a `::(...)`/`[...]` spelling that always needs inspecting) so a
   program using neither type captures nor package-scoped nested types -- the
   overwhelming majority of type checks -- skips the call entirely instead of
   re-deriving the same "nothing to resolve" verdict on every single type check.

Both steps are pure refactors of how the existing logic reports "nothing to
resolve"; behavior is unchanged. Verified with `valgrind --tool=callgrind` in an
isolated worktree at the regressing commit: 2,202,223,143 Ir before, 1,925,241,084
Ir after (baseline: 1,895,006,044 Ir) -- recovering ~90% of the regression, down
from +16.2% to +1.6%.

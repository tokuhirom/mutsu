# Fixed a per-call String allocation in every routine call

`Interpreter::unit_of_source_sym` compared a routine's declaring file against
`$*PROGRAM` with `Symbol::resolve()`, which allocates a fresh `String` on
every call. `Symbol::as_str()` performs the identical comparison with no
allocation, and mutsu already documents `resolve()` as "prefer `as_str()` on
hot paths".

Before PR #9627 this branch was effectively unreachable on a plain script's
hot path, because `CompiledFunction::source_file_sym()` returned `None` for
an ordinary top-level `sub`. #9627 widened `source_file_sym()` to fall back
to the routine's own `source_file`, which made it return `Some(...)` for
essentially every routine — turning every call in every mutsu program into an
allocating comparison, not just the `%?RESOURCES` cases #9627 was fixing.

`scripts/bench-det.sh` confirms allocation counts back at their pre-#9627
baseline: `fib` 262,669 → 20,545, `bench-fib` 656,241 → 21,313, `bench-tak`
1,604,648 → 1,209,556, `debug-guard` 620,759 → 521,430.

A new deterministic regression test
(`tests/source_file_sym_fallback_alloc_budget.rs`) pins the fix with a
thread-local counting allocator: a recursive call must not allocate to find
its own compilation unit.

# The `@`/`%` shared-var lane freezes a name once two bindings reuse it

The name-keyed `shared_vars` store (`src/runtime/shared_store.rs`) is what keeps
a parent and a `start` worker on ONE array object, and `@`/`%` names stay on it
by design (`docs/recursive-start-shared-vars.md`). But it is keyed by the bare
name and seeded once (`seed_if_absent`), so it cannot represent two
*concurrently-live* bindings of one name. When a second, unrelated binding of the
same `@` name later feeds a spawn, the worker reads the first one:

```raku
say (await map -> [$a, @K] { start { "$a:{@K[0]}" } }, (1, (100,101)), (2, (200,201))).join('|');
say (await map -> @K       { start { @K[0] } },        (300, 301), (400, 401)).join('|');
# raku:  1:100|2:200   then  300|400
# mutsu: 1:100|2:200   then  100|100      <- the second line reads the FIRST line's @K
```

The second line is correct on its own (`300|400`) and correct with a different
name (`@P`). It only breaks after some earlier binding has already seeded `@K`.
The `$`-sigil case does not have this problem: those names were taken off the
lane entirely and given a per-binding home (PLAN.md §6).

The destructuring-sub-signature half of this was fixed in
`news/2026-08/start-block-destructured-array-param.md` by keeping *those*
bindings off the lane (they are provably per-invocation). What remains is the
general case: two ordinary bindings of one `@` name, at least one of which is
captured by a spawned block.

## Why it is large

The real fix is the one `docs/recursive-start-shared-vars.md` defers: give
`@`/`%` captures a per-binding home like `$` has, instead of the bare-name lane.
That is blocked on the `__mutsu_atomic_*` CAS copies, which are keyed off these
very entries, and on `push_to_shared_var` / the element-assign write-throughs,
which decide "is this name genuinely shared?" by *presence* in the store. Pinning
`t/concurrency-threading.t` test 4 (`my $c = Channel.new; start { $c.send(42) }`)
showed the same lane is load-bearing for non-`$` shapes that
`box_captured_lexicals` declines to box.

A narrower stopgap would be to make each *fresh binding* of an `@`/`%` name
`declare` into the current lineage (shadowing the ancestor entry) rather than
leaving the stale one visible — the mechanism `thread_redeclared_vars` already
uses for `my`. That was tried for the sub-signature case and moved the error
rather than removing it: the lane then holds the *latest* binding, so an earlier
still-running worker reads the newer value.

## Minimal repro

The two-line program above. Reproduces deterministically; no timing involved.

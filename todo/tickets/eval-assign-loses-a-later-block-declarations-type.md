# An EVAL'd by-name assignment stops seeing a later `my Int $x`'s type

Raku's "declarations are in effect at block start" rule means an assignment that
runs *before* the textual declaration still sees its type constraint:

```raku
{
    try { EVAL '$x = "abc"' };   # X::TypeCheck::Assignment: expected Int, got Str
    my Int $x;
}
```

That still works in isolation. Inside `roast/S04-declarations/my-6e.t` (and its
`6.c` twin `roast/6.c/S04-declarations/my-6c.t`) it no longer does when the file
runs under the **vendored upstream `Test`** module (`MUTSU_REAL_TEST=1`): the
EVAL'd assignment succeeds silently and the assertion

```raku
dies-ok { EVAL '$x = "abc"'; my Int $x; }, 'also a type error';
```

fails. Instrumenting the same spot to report what was thrown:

```
native provider:  X::TypeCheck::Assignment: Type check failed in assignment to
                  $x; expected Int but got Str ("abc")
real Test:        NO-DIE
```

## Bisected

Introduced by **`e49b300` / PR #7364, "retire the global name-keyed
type-constraint side table (ADR-0042 slice 3)"**. Verified by building both
sides on a debug build and running the file under `MUTSU_REAL_TEST=1`:

| commit | `my-6e.t` under the real `Test` |
| --- | --- |
| `bbdebb1` (the merge before it) | passes |
| `e49b300` (ADR-0042 slice 3) | `not ok 61 - also a type error` |

The native provider passes on both, which is why CI did not catch it: the
dual-provider sweep (`scripts/roast-test-module-sweep.sh`) is not part of CI.
See `todo/deep/vendor-real-test-module.md`.

## Why the provider matters at all

The construct is provider-independent in isolation — the bare block above is
correct on current `main` — so the failure needs the file's accumulated state.
The plausible reading is that the retired side table was what carried a
*name*-keyed constraint for a declaration whose container does not exist yet
(nothing has executed the `my Int $x`), and that after the retirement the
constraint is only reachable through a container/descriptor that some earlier
statement in the file happens to have materialized. The two providers run
different sequences of EVALs before reaching line 229, so only one of them
leaves that state behind. That is a hypothesis, not a diagnosis: it has not
been confirmed against the slice-3 diff.

## Reproducing

```sh
cargo build
MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 ./target/debug/mutsu roast/S04-declarations/my-6e.t
# not ok 61 - also a type error
```

For a copy you can edit (roast is read-only), take the file's first 229 lines,
drop its `plan`, close the block and add `done-testing`, then run it with
`-I roast/packages/Test-Helpers/lib`; the failure survives that truncation, so
bisecting the preceding statements is tractable from there. It does NOT survive
extracting only the enclosing block, so the trigger is cumulative.

## Why this is a ticket and not a fix

The right fix depends on what ADR-0042 slice 3 intends to replace the name-keyed
lookup with for a declaration that has not executed yet — a question for the
slice's own design rather than something to patch around at the EVAL assignment
site. Both affected files are on the roast whitelist and pass under the default
(native) provider, so nothing is currently red; this blocks completion criterion
1 of `todo/deep/vendor-real-test-module.md` (no real-provider correctness
regressions), which is otherwise now met for `t/` — that sweep reports **0**
regressions as of 2026-09-06.

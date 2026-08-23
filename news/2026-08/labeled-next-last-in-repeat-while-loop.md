# `next LABEL` / `last LABEL` inside a labeled `repeat {} while` loop throws X::ControlFlow

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/js-nutshell.rakudoc:613`).

## Root cause hypothesis

A `repeat {...} while COND` (or `repeat while COND {...}`) loop's own control-flow
handling recognizes unlabeled `next`/`last`, but does not recognize a `next`/`last`
that targets the loop's own label. The labeled control-flow exception propagates past
the `repeat` loop's catch site (which apparently only matches unlabeled `next`/`last`,
or matches on some internal loop-id that isn't wired up for `repeat`), so it escapes as
an uncaught `X::ControlFlow` runtime error instead of being caught and handled by the
labeled loop.

Every other labeled loop form (`for`, `while`, `loop`) handles a labeled `next`/`last`
targeting itself correctly; only `repeat` reproduces this.

## Minimal repro

```raku
OUTSIDE: repeat { next OUTSIDE; } while False;
say "done";
```

- `raku`: prints `done`
- `mutsu` (`target/debug/mutsu`): `Runtime error: X::ControlFlow`

Also fails with `last OUTSIDE;` in place of `next OUTSIDE;`. An unlabeled `next;` (no
label) inside the same `repeat` loop works fine — only the labeled form is broken.

The original doc example nests this inside a `for %primes.keys` statement-modifier,
but the label/repeat interaction alone reproduces it without any `for`.

## Affected files (starting point)

Loop control-flow handling in `vm/vm_control_ops.rs` (the `repeat`/`while`/`until`
loop execution path) — compare how `next`/`last` label matching is wired for `for`
loops vs. `repeat` loops.

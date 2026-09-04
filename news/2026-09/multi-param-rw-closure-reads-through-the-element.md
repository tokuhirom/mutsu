# A read-only closure over a multi-parameter `is rw` loop reads through the element again

A `for` loop parameter binds the element's container (ADR-0045), so a closure
created in the body reads *through* it: a later write to the element is visible.
Every single-parameter form did that. A **multi-parameter** loop lost the read
direction while keeping the write direction:

```
$ raku  -e 'my @a=1,2,3,4; my $c; for @a -> $x is rw, $y is rw { $c = -> { $x } if $x==1 }; @a[0]=99; say $c()'
99
$ mutsu -e '...same...'          # before
1
```

The split was the tell. A closure that *writes* `$x` read `99` inside itself, so
the alias was real; only a closure that read `$x` and nothing else lost it. That
is exactly the by-value-capture hazard CLAUDE.md's "what gain and risk actually
mean" section describes.

## Root cause: an element cell frozen as a per-iteration snapshot

Not `resolve_capture_slot` or `compute_owned_captures`, which the finding
suspected — both were already handing the closure the live cell. Instrumenting
the capture, the closure's stored env and its entry showed:

| | at capture | at call |
| --- | --- | --- |
| read-only closure | `ContainerRef` | plain `1` |
| writing closure | `ContainerRef` | `ContainerRef` → `99` |

The step in between is `freeze_readonly_owned_captures`
(`src/vm/vm_register_ops.rs`). It exists for ADR-0027's per-iteration identity:
a loop-body `my` that was boxed into a cell is *one* cell reused across
iterations, so a read-only closure must freeze its own iteration's value rather
than read the loop's last one. It deep-derefs the cell and skips only captures
the closure mutates — hence the read/write split.

An **rw loop parameter** is the one binding that must not be frozen: it is the
source element's own container, and a fresh one per iteration, so there is no
last-iteration value to defend against and the element is expected to change
from outside the closure entirely. A single-parameter rw loop never reached the
freeze at all — it binds natively and is not registered as loop-local — while a
multi-parameter one binds through `build_for_bind_stmts`' declaration prefix,
which *is* an ordinary declaration and does register. So the two forms disagreed
purely by how the parameter got bound.

## The fix

The loop now records the bare names it binds as genuinely rw
(`Interpreter::active_loop_rw_param_names`, pushed and popped with the loop-local
scope and saved/restored across call frames exactly like its non-rw sibling
`active_loop_param_names`), and `freeze_readonly_owned_captures` leaves those
alone.

**It has to be runtime-scoped, not a per-`CompiledCode` name set.** That was
tried first and measured wrong: names are reused across the loops of one
compilation unit, so one loop's `is rw` exempted an unrelated later loop's
same-named *non-rw* parameter and cost it its per-iteration identity. Only *rw*
parameters are exempt — a non-rw parameter copies, so its freeze is correct and
stays.

## Pins

Four rows in `t/for-loop-element-alias.t` (ADR-0045 §1.3 rows 11/20, the read
half for the multi-parameter shapes): the plain `is rw` multi-param and the
`.kv` value slot now read `99`, and the two negatives — a non-rw `.kv` value
slot and a non-rw multi-param — still read the iteration's own value.

## Found along the way

Two `for` loops in one compilation unit that name their parameters the same do
not behave independently: an earlier `is rw` loop changes what a later,
unrelated, non-rw loop's closures capture (`[30 30]` where raku says
`[10 30]`). Pre-existing on `main`, filed as
`todo/tickets/same-named-loop-params-in-one-unit-interfere.md`. It is why the
two negative rows above name their parameters `$m`/`$n`/`$w` instead of the
natural `$x`/`$y`/`$v`.

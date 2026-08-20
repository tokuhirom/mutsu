# A `start` block nested inside a `whenever` body no longer loses its writes

A `start` created directly inside a `react { whenever ... { ... } }` body used
to silently lose any write it made to an outer lexical:

```raku
my $x = 0; my $p;
react { whenever Promise.in(0.03) { $p = start { $x++ } } }
await $p;
say $x;   # raku: 1   mutsu (before): 0
```

## Root cause

`Stmt::Whenever` compiles an analysis-only copy of the whenever body
specifically to detect cross-thread lexical captures ("Case B"), and stashes
its index on the opcode as `OpCode::WheneverScope { analysis_cc_idx, .. }`. But
the VM dispatch destructured this field as `analysis_cc_idx: _` and
`exec_whenever_scope_op` never used it, so `box_captured_lexicals` was never
called for a whenever body's enclosing frame -- unlike its twin mechanism for
`gather` (`MakeGather`), which does call it correctly. The analysis was
computed on every compile and thrown away at dispatch.

Without that boxing call, a lexical the whenever body's enclosing frame owns
stayed a plain (non-`ContainerRef`) value in the env, so a `start` block
created inside the whenever body captured it by value instead of sharing a
cell -- the worker's write updated its own private snapshot and the parent
never saw it, even after `await`ing the very promise that performed the write.

## Fix

Threaded `analysis_cc_idx` through the dispatch into
`exec_whenever_scope_op` (mirroring `MakeGather`'s exact pattern), and added a
`self.box_captured_lexicals(code, &analysis_cc)` call at the top of
`exec_whenever_scope_op`, before `run_whenever_with_value` clones the env for
the callback closures. This boxes the enclosing frame's lexicals into shared
`ContainerRef` cells first, so the whenever callback's env clone -- and any
`start` block nested inside it -- carries the live cell.

Verified against `raku` across 15 shapes: the newly-fixed direct case, a
`start` nested one level deeper inside a `for`/`map` within the whenever body,
an `@`-element write from inside the nested `start`, and the `gather` twin
(already correct) as a non-regression -- plus the full negative-control matrix
from the original filing (`start` in mainline, in `map`/`for`/`if`/`while`
bodies, inside another `start` three deep, inside a `supply` block, inside a
named sub called from a `whenever`, and closures stored in variables or passed
as arguments and invoked on a worker), none of which regressed.

Perf: a hot-loop micro-benchmark that re-executes `WheneverScope` 3000 times
(re-tapping a trivial `whenever` inside a channel-driven loop) showed no
measurable regression (1.37s before vs 1.44s after, within run-to-run noise) --
`box_captured_lexicals`' early-out makes the extra scan effectively free when
the enclosing frame has no `needs_cell_locals`.

New pin: `t/whenever-body-closure-cross-thread-cell.t` (12 subtests covering
the fixed shapes and the negative-control matrix).

See `todo/tickets/whenever-scope-discards-its-analysis-cc.md` (now resolved) for
the full investigation. A separate, unrelated scheduling defect was found while
verifying this fix's expected side effect on
`t/react-nested-whenever-on-demand-close.t` (that file's 5s backstop is not
caused by this bug -- `Supply.on-demand`'s `closing` callback is deferred to
react teardown instead of firing per-tap); filed as
`todo/tickets/supply-on-demand-closing-callback-deferred-to-teardown.md`.

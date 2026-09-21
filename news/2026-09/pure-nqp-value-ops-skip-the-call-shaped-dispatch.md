# The pure `nqp::` value ops stop paying for a call

In NQP/Rakudo an `nqp::` value op is a `QAST::Op` node the compiler turns into
ONE MoarVM instruction: `nqp::add_i($a, $b)` is an `add_i` against two native
registers. mutsu reached the same answer through a call-shaped protocol — build
an argument list, clear the pending callsite line, publish and restore the
multi-candidate literal mask, dispatch on the op's identity, then match its NAME
inside the owning table.

[#8900](https://github.com/tokuhirom/mutsu/issues/8900) sized `nqp::` op bodies
at 16.9% of a `JSON::Fast` decode. Measuring what sits *around* those bodies
turned out to be the more interesting half. On the ADR-0110 §8 decode benchmark
(200 records, callgrind, differencing 1 vs 10 decodes so startup and module
compilation cancel), one decode runs **223,813 `nqp::` ops at ~973 instructions
each**, of which about **360 are spent before the op body starts**:

| step | Ir per op |
| --- | ---: |
| `exec_nqp_op`'s argument-list prologue and epilogue | ~180 |
| `dispatch_nqp_op_by_id`'s id → name → table select | ~38 |
| the owning table's `match op { .. }` over its names | ~145 |

1,595,870 of the 2,508,919 ops in the 10-decode run are the Value table's
integer and float ops — `iseq_i`, `add_i`, `sub_i`, the comparisons — whose
bodies are a single machine operation. The protocol was two orders of magnitude
more expensive than the operation.

## What changed

`src/runtime/nqp_pure.rs` now holds the single implementation of each `nqp::` op
that is a pure function of native operands. `call_nqp_op`'s string-keyed arms
call it, and `exec_nqp_op` reaches it directly: one array load keyed by the op's
compile-time id decides whether the op qualifies, and the operands are read where
the caller's opcodes left them, on the stack. No argument vector, no name, no
table walk. TRIR's `NqpOpGen` uses the same table.

Two properties keep the two paths from drifting, both structural rather than
maintained by hand:

- **One body per op, not two.** `nqp_pure::eval` is what the string-keyed table
  runs as well.
- **The direct path declines every operand shape it has not proved.** It runs
  only when the call site's arity matches the op's own and every operand is
  already exactly an `Int` (or, for a `_n` op, an `Int` or a `Num`) — a pure
  NaN-box tag probe. A `VarRef`, a `ContainerRef`, a `HashEntryRef`, a `Proxy`
  and a `Str` all fail it and keep the general path, which stays the only place
  the unwrapping, container deref, `Proxy` FETCH and coercions live. So the
  direct path never has to reproduce any of them.

`div_i`/`mod_i` are deliberately outside the set: they can raise, so they are not
total, and they keep the general path where that error is raised.

## The result

| | marginal Ir per decode |
| --- | ---: |
| before | 2,309,159,750 |
| after | **2,279,220,971** |
| | **−1.30%** |

| | before | after |
| --- | ---: | ---: |
| `nqp::` ops executed (10 decodes) | 2,508,919 | 2,508,919 |
| ...reaching `dispatch_nqp_op_by_id` | 2,508,919 | **1,366,743** |
| ...reaching the Value table's string match | 1,595,870 | **453,694** |

**45.5% of all `nqp::` ops no longer touch the dispatch machinery at all.** Self
costs move accordingly: `call_nqp_op` −55.7%, `dispatch_nqp_op_by_id` −45.3%,
the drain/map iterator adapters −39% to −43%.

Wall clock is not quoted, because 1.3% is below what this box can resolve
(`.agents/skills/perf-tuning/SKILL.md` §0: a change under ~2% will not show in
paired runs). The instruction counts are the evidence; the claim is that the
interpreter does strictly less work per `nqp::` op, not that a stopwatch notices.

## Why it is 1.3% and not the 16.9% #8900 names

Because the remaining 1.37 M ops per 10 decodes are the ones that *touch
containers* — `atpos_i`, `bindpos_i`, `push_s`, `bindkey`, `findnotcclass`,
`getattr` — and their cost is in the body, not the protocol. The dispatch step
really is ~360 instructions of pure overhead per op, and removing it really does
help every `nqp::`-heavy module; it just applies to slightly under half of them.
#8900 stays open: deciding when its scope is satisfied is the maintainer's call,
and its second half (the general binder, 16.1%) is untouched here.

One finding from the same profile was filed separately rather than folded in:
[#8999](https://github.com/tokuhirom/mutsu/issues/8999) —
`unicode_general_category` runs up to 28 regex matches and allocates a `String`
per character, which is 970 instructions a character under
`nqp::findnotcclass`, and sits under grapheme segmentation, collation and
`.uniprop` as well.

## One thing nearly got dropped

The first cut of the direct path skipped `set_pending_callsite_line(None)`, on
the argument that a pure op dispatches nowhere and so cannot reach a test
assertion. That reads the marker backwards: it is a ONE-SHOT line a *preceding*
call leaves for the *next* reader, and clearing it on every `nqp::` op is what
makes it one-shot. The JIT's inline form of these same ops (`emit_nqp_int_binop`,
ADR-0004 J4) stores the `None` by hand and says exactly why — the interpreter's
counterpart owes the same store, which is one field write and 0.07% of the win.
When a second implementation of something already exists, its comments are
worth reading before deciding which of its steps are unnecessary.

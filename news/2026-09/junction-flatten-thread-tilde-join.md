# Junctions no longer nest through `~`, and thread through `.join`

Two related junction-threading gaps from the 2026-09-09 doc-diff sweep.

## `~` over two same-kind junctions nested instead of producing one junction

`(1|3|5) ~ (2|4|6)` answered `any(any(12, 14, 16), any(32, 34, 36), any(52,
54, 56))` instead of Rakudo's flat `any(12, 14, 16, 32, 34, 36, 52, 54,
56)`. `eval_concat_with_junctions` (`src/vm/vm_coerce_concat_ops.rs`) threaded
over the left junction's values while passing the right operand through
unchanged as a whole `Junction`; the recursive call then threaded over the
right junction too, so each of the left's results came back already wrapped
in its own inner junction, and the outer `map` wrapped that again — a
junction of junctions instead of one flat cross product.

Fixed by detecting when both operands are junctions of the **same** kind
and threading the full cross product directly into one flat `Junction`,
before the existing left-first threading (used only when kinds actually
differ, e.g. `(1|2) ~ (3&4)`) runs. Verified against `raku` that this
same-kind flattening is specific to `~` — `+`, `*`, `==`, `eq`, and every
other operator sharing the general `eval_binary_with_junctions` helper keep
same-kind junctions genuinely nested in real Rakudo, so that shared helper
was deliberately left untouched (an earlier attempt to "fix" it there was
reverted after `(1|3) + (2|4)` stopped matching `raku`).

## A junction inside a list did not thread through `.join`

`("a"|"b", "c", "d").join` answered `any(a, b)cd` — mutsu stringified the
junction element in place instead of threading the whole `join` over its
eigenstates, where Rakudo answers `any(acd, bcd)`.

`join` turned out to have four separate implementations (a native 2-arg
fast path, a native 1-arg method-call fast path, a native 0-arg method-call
fast path, and the interpreter's own slow-path `dispatch_join_method`), none
of which recognized a `Junction` among the elements to join. Added a new
pure helper, `thread_junctions_in_items` (`src/builtins/functions/flat.rs`):
given an already-flattened list, if it contains a `Junction`, thread the
join across the cross product of every same-kind junction position's
eigenstates, returning one flat `Junction` (mismatched kinds still nest,
mirroring the `~` fix above). Wired into `builtin_join` and
`dispatch_join_method` (the two places that actually render the joined
string), and added a `Junction` fall-through gate to the three native fast
paths (`join_needs_interpreter`, and the 0-arg/1-arg method dispatch arms)
so a junction-containing call reaches one of those two implementations
instead of answering directly.

Both fixes verified against `raku`: the two numbered repros, width-correctness
(`(1|3) ~ (2|4)` has four eigenstates), the previously-correct nested-form
comparison control, the mismatched-kind-stays-nested control, and the
unaffected-arithmetic-operator control. Pinned by
`t/issue-7755-junction-flatten-thread.t`. All 20 existing
`t/*junction*.t` files (226 assertions) continue to pass unchanged.

A mixed-kind junction combined with **another** junction inside the same
list (e.g. `("a"|"b", "c"&"d", "e").join`) is not fully fixed — Rakudo's
exact kind-label-swap for that deeper case was not reproduced, matching the
issue's own guidance not to blindly extend into unverified territory. Two
further, separately-mechanismed junction-autothreading gaps the same sweep
found (`%h{one <foo meow>}:exists`, and a junction rebuilt through `(gather
$j».take).grep`) were recorded in the issue but are explicitly out of scope
here.

Closes #7755.

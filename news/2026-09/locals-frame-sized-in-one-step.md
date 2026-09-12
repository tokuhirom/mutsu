# Opening a callee's frame in one step, and what Slice 2 really cost

ADR-0077 Slice 2 gave every live frame's slots one contiguous stack and deleted
the locals pool. This is the measurement it owed, and the defect the measurement
found.

## The bench CI could not answer the question

Slice 2 merged as `e5c260c`, and its `bench-history.tsv` rows read like a win:
`bench-fib+jit` 0.1345 → 0.1306 s, `bench-tak+jit` 0.1690 → 0.1583,
`method-call+jit` 0.1453 → 0.1329, `bench-ctor+jit` 0.3472 → 0.2341. All of it
is noise. `bench-ctor+jit` reads 0.3472 / 0.2341 / 0.3495 / 0.2326 / 0.3475 /
0.3604 across six consecutive main commits — a ~48% bimodal swing with nothing
in those diffs to explain it — and plain `bench-fib` jumps 13% on an unrelated
commit as well. The `runner` column says `4c-x86_64-ubuntu24` for every row, so
this is not the host-class caveat PERFORMANCE.md already records; the same
class evidently spans hosts differing by more than the change under measurement.

A few-percent single-commit change is therefore below this series' resolution,
and no number of further rows fixes that. What the series can still do is catch a
large regression, and it showed none.

## Retired instructions, which can answer it

callgrind on `--profile profiling` builds of the merge commit and its parent,
`fib(22)`, 57 312 calls, output verified identical (17711) before comparing:

| build | JIT on | | JIT off | |
| --- | ---: | ---: | ---: | ---: |
| `9cf38ab` (pre-Slice-2) | 152 721 942 | — | 260 873 311 | — |
| `e5c260c` (Slice 2 as merged) | 152 831 725 | **+0.07%** | 259 743 537 | **−0.43%** |
| one-step frame sizing (this change) | 149 633 356 | **−2.02%** | 253 330 506 | **−2.89%** |

**Slice 2 as merged bought nothing.** Deleting the pool removed `recycle_locals`
(2.98M Ir, 1.95% inclusive) as designed — and the migration handed most of it
straight back. It opened each callee frame in two steps: `push_frame(0)` at the
point where the caller's slots have to stop being visible, then
`refill_slots(num_locals)` once the callee's size was known. So every call paid
**two** out-of-line `Vec::resize` / `extend_with` calls where the pool had paid
one, and `Vec::resize` inclusive went 3.11M → 5.28M: +2.17M against the 2.98M
saved.

The fix is to open the frame at its real size once. `num_locals` is available at
the save point in all three light call paths, and nothing between there and the
parameter bind reads `self.locals`, so the two steps collapse into
`push_frame(num_locals)`. `Vec::resize` is back to 3.10M. `push_frame` and
`refill_slots` now also skip the resize entirely when the frame is empty, which
is what every `push_call_frame` opens.

## What this says about the rest of the campaign

The remaining cost is exactly where ADR-0077's callgrind cross-check put it:
`resize` itself, about 2% — filling a fresh frame with `Nil`. That is the half
the *leading-parameter* form removes, because an argument already sitting in the
cell its slot wants needs no fill at all. Slice 2 is the precondition for that
work, not a substitute for it, and this measurement is what makes the difference
legible instead of assumed.

It is also a reminder about profile-driven work in general: a cluster that
disappears from a profile has not necessarily left the program. `recycle_locals`
really was gone from the symbol list after Slice 2, and the instructions had
simply moved into a symbol that was already there.

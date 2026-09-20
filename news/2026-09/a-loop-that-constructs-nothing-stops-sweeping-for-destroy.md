# A loop that constructs nothing stops sweeping for DESTROY

Every loop iteration in every program asked the thread-local `DESTROY` queue
whether it had anything in it, and asked by **taking** it. In a `while` loop
that constructs no objects at all that cost **56 instructions per iteration**,
3.6% of the loop. One relaxed atomic load answers the same question.

## What was happening

`exec_while_loop_op_inner` runs `run_pending_instance_destroys()` at every
iteration boundary, "mimicking GC-like behavior so DESTROY fires during
execution". That function began with:

```rust
let pending = take_pending_instance_destroys();
if pending.is_empty() { return Ok(()); }
```

`take_pending_instance_destroys` is `PENDING_INSTANCE_DESTROYS.with(|p| mem::take(&mut *p.borrow_mut()))`
— a thread-local access, a `RefCell` borrow, a three-word `mem::take`, and the
construction and drop of an empty `Vec`, all to learn that nothing was queued.

Measured on `while nqp::islt_i($i, 100000) { $i = nqp::add_i($i, 1) }`:
**5,600,641 Ir over exactly 100,000 calls**, in a loop that never constructs an
object.

## What it does now

The whole-program latch answers it instead:

```rust
if !crate::value::any_destroy_method_declared() { return Ok(()); }
```

The soundness is not an argument about the queue, it is symmetry with the one
site that fills it. `InstanceAttrs::finalize_destroy` — the single place an item
is ever pushed — already checks **this same latch** before pushing:

```rust
// Nothing in this program declares a user `DESTROY`, so the queued item
// could only be walked and thrown away.
if !super::any_destroy_method_declared() { return; }
```

So a clear latch is a proof that no thread has ever queued anything. And the
latch's monotonicity is the contract `ANY_DESTROY_DECLARED` already documents:
it is read at *drop* time rather than at construction, precisely so that a
`DESTROY` registered later (an `.^add_method`, an `EVAL`ed class) still fires
for everything that dies after it. That is pinned by
`t/vm/destroy-latch-late-registration.t`, which is exactly the invariant this
change rests on.

The non-empty half moves out of line behind `#[cold] #[inline(never)]`, so
inlining the check into a loop body copies one load and a branch rather than the
whole sweep.

## Measurements

Baseline `ff52eacb`. `--profile profiling` builds of the same tree with and
without the change, first run after each build discarded.

| | before | after | |
| --- | ---: | ---: | ---: |
| `nqp::islt_i` + `nqp::add_i` loop | 157,774,687 | **151,674,056** | **-3.87%** |
| plain `$j = $j + 1` loop | 225,014,090 | **218,914,072** | **-2.71%** |
| `bench_json.raku`, 100 records | 1,844,183,367 | 1,844,209,280 | +0.00% |

Both loops drop by ~6.1M, or **61 instructions per iteration** — the 56 the
profile predicted, plus the call itself. The JSON parse does not move because
its hot path is the recursive descent rather than a loop body, so it crosses
far fewer iteration boundaries.

Unlike the last two entries this is not a targeted win: it applies to every
`while` and `for` iteration in every program that does not declare a user
`DESTROY`, which is almost all of them.

## How it was found

Not by looking for it. It fell out of the profile taken to decide what to do
about `SetLocal` after the nqp inline landed — `run_pending_instance_destroys`
sitting at 3.6% of a numeric loop is not something anyone would think to
suspect, and is only visible once the things that *were* expected to dominate
have been removed.

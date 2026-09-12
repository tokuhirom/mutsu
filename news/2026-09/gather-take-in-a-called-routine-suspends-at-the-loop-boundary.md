# A `gather` whose `take`s live in a called routine is lazy again

`gather { loop { self!bitmap(...) } }` — an unbounded `gather` whose body does nothing but call a
routine that `take`s — ran forever under mutsu. Pulling five elements out of it never returned.

## Why it ran forever

The lazy-pull driver (`force_lazy_list_vm_n_inner`) executes the gather body's own compiled code and
can snapshot and resume only that one frame. A `take` that fires inside a routine the body *called*
therefore cannot suspend at the take itself: raising the take-limit signal there would unwind the
callee and leave the driver's saved ip pointing at a call op whose arguments have already been
drained. `take_value` recognised that case — it compares the live call depth against
`lazy_pull_entry_call_depth` — and dealt with it by giving up on suspension entirely and collecting
eagerly, on the reasoning that over-production is still correct.

Over-production is correct only for a body that ends. When every `take` in an *infinite* body comes
from a nested call, nothing ever asked the body to stop, so the driver collected until the process
died.

## The fix

A nested `take` cannot suspend where it fires, but it does not have to. A condition-driven loop in
the driver's **own** frame reaches its next iteration boundary only after the callee has returned,
and that boundary is already a sound suspension point — it is the one `gather_suspend_pending`
exists for, used when a straight-line `take` fires inside a `while`/`until`/C-style `loop`.

So a nested take now parks `gather_suspend_pending` instead of silently declining to suspend, and
the two sites that consume the flag (`exec_while_loop_op_inner`, `exec_cstyle_loop_op_inner`) ask
`gather_suspend_boundary_reached()` rather than reading the flag directly. That helper honours the
flag only for a loop running at or above the pull's entry call depth; a loop *inside* the callee
leaves it set and keeps running, so the flag survives the callee's return and the gather body's own
loop consumes it one boundary later.

Nothing about the sound-suspension rule changed — the signal is still never raised from a callee
frame. What changed is that declining to suspend at the take no longer means declining to suspend at
all.

## What it moves

`t/collections/lazy-seq/gather-take-in-callee-infinite-loop.t` pins nine shapes: a bare sub and a private
method called from an infinite `loop`, the same under `while`, a take two call levels down, a take
inside a loop in the callee (still sound, may over-produce, but terminates), two finite gathers that
must keep their exact contents, and one check that a suspended pull does not truncate a later
unrelated `gather`. rakudo agrees with all nine; before the fix the first one hung.

In the ecosystem parity corpus this is `EuclideanRhythm`'s whole suite. Its `method list()` is
`gather { loop { self!bitmap($!level, @!count, @!remainder) } }`, an infinite lazy pattern the tests
slice with `$obj.list[^16]`. `t/020-basic.t` went from a sweep timeout producing nothing to all 68
assertions passing, and the distribution from `partial` to `green`.

## Found by

The ecosystem parity sweep's `timeout` cluster,
[#7995](https://github.com/tokuhirom/mutsu/issues/7995) — 20 distributions that rakudo finishes
inside the sweep budget and mutsu does not. Splitting that cluster into "slow but finishes" and
"never finishes" is what surfaced this; the other never-finishing members turned out to be four
separate correctness bugs, filed as
[#8046](https://github.com/tokuhirom/mutsu/issues/8046),
[#8047](https://github.com/tokuhirom/mutsu/issues/8047),
[#8048](https://github.com/tokuhirom/mutsu/issues/8048) and
[#8049](https://github.com/tokuhirom/mutsu/issues/8049).

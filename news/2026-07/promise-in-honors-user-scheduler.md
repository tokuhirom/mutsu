# `Promise.in` / `Promise.at` honor a user-defined `$*SCHEDULER`

Raku defines `Promise.in($t)` as `$*SCHEDULER.cue({ ... }, :in($t))`. Replacing
`$*SCHEDULER` is therefore how a test drives *virtual* time — the whole premise of
the `Test::Scheduler` distribution, where `Promise.in(200)` must not resolve until
`$*SCHEDULER.advance-by(200)` is called, no matter how much real time passes.

mutsu never consulted `$*SCHEDULER` at all: `Promise.in`/`Promise.at` went
straight to the shared deadline-heap timer, so swapping the scheduler had zero
effect and a promise resolved on real time regardless.

Both now route through the dynamic scheduler when it is a **user-defined** class —
one that is not `ThreadPoolScheduler` / `CurrentThreadScheduler` / `FakeScheduler`
and that provides a `cue` method. The built-in schedulers keep the direct
deadline-heap path, which is much cheaper and observationally identical, so
ordinary `Promise.in` pays nothing. `Promise.at` cues as `:in($at - now)`, matching
Rakudo (verified against `raku`), not as `:at`.

The callback handed to `.cue` is a synthesized zero-argument block whose body is
`$promise.keep(True)` — a first-class `Callable` the user scheduler can store and
invoke whenever its own clock says so.

## A second bug this uncovered

`Scheduler` is a composable built-in role (`class Test::Scheduler does Scheduler
{...}`), but its class entry claimed a native `cue`. Since `is_native_method` walks
the MRO, *every* user class composing the role looked native-backed, so its own
`method cue` was bypassed and dispatch died with
`No native method 'cue' on 'MyScheduler'`. There is no native implementation keyed
on bare `Scheduler` — only the three concrete schedulers have one, and each lists
`cue` itself — so the role no longer claims it.

Found while triaging `TODO_dist` ticket T-037 (Test::Scheduler).

Pin: `t/promise-in-honors-scheduler.t` (9 assertions, passes under both mutsu and
raku).

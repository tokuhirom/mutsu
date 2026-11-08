# ADR-15967: Whether `start` blocks must begin in submission order

- **Status**: Proposed (needs the maintainer's call; recommendation in §4)
- **Date**: 2026-11-08
- **Deciders**: ecosystem inventory worker, for the maintainer's review
- **Issue**: [#15967](https://github.com/tokuhirom/mutsu/issues/15967); related
  [#14684](https://github.com/tokuhirom/mutsu/issues/14684) (same test file),
  [#15077](https://github.com/tokuhirom/mutsu/issues/15077) (`start` start-up latency),
  [#16898](https://github.com/tokuhirom/mutsu/issues/16898) (Object::Delayed, a different
  cross-thread ordering assumption).
- **Related**: [ADR-0123](0123-bounded-user-thread-stack-reservations.md),
  [ADR-16595](16595-threads-reach-lexicals-through-their-captured-cells.md)

## 1. Context

MCP::Server `t/21-elicitation-dispatch.rakutest`, subtest "A round that answers only some of the
questions", starts two `start` blocks back to back and assumes the first one asks first. Under
mutsu it fails about 1 run in 6-9; rakudo has not been seen to fail it in 3 runs. The pool is a
FIFO queue, so the *dequeue* order is the submission order; what varies is which worker thread
reaches its first user statement first (thread wake-up, closure setup, lock acquisition).

A previous attempt (branch `fix/15967-start-blocks-begin-in-submission-order`, no PR; see the
Re-triage comment on #15967) added a queue gate (an ordered task waits until its predecessor
began) and a worker gate (a hook at the block's first statement). Measured on a release build
over 300 iterations: failures went from 4-8 to 0-6 per run, i.e. about half, never to zero, and a
burst of 20,000 `start { $_ + 1 }` got about 20% slower.

Neither the language documentation (`raku-doc/doc/Type/Promise.rakudoc`, `Language/concurrency`)
nor roast promises that two `start` blocks *begin executing user code* in submission order. They
promise the code runs eventually and that `await` observes its result.

## 2. Options

**(a) Ship the gates plus a grace timer.** Ordered start with a short hand-off latency. Costs a
tax on every `start` of a burst; still not a guarantee without (b).

**(b) A VM hook that releases the gate at the first statement boundary or blocking point.** The
only way to make the first *statement* ordered. Costs a check per statement or a compile-time
marker after the first statement of every `start` body; serialises a CPU-bound first statement
(`start { heavy() }` twice would run one after the other) unless a blocking point also releases.

**(c) Keep rakudo's model: no ordering guarantee.** Rakudo has the same race and merely loses it
much less often, because its thread-pool hand-off is faster. Reduce the window instead of closing
it: make `start` begin running faster (#15077: mutsu 1.5-10 ms vs rakudo 0.1-3 ms), which is a
plain `todo:perf` goal with no new semantics.

## 3. Consequences

(a) and (b) add a semantic mutsu would have to keep forever (a guarantee rakudo does not give,
so no program written for rakudo can rely on it, and a program written for mutsu would not run
elsewhere) and put a cost on the hottest concurrency primitive. (c) leaves the MCP::Server
subtest flaky at a low rate; that is the test's assumption, not the language's.

## 4. Recommendation

Choose **(c)**: close the ordering question as "no guarantee, as rakudo", drive #15077 (start
latency) as the mitigation, and treat the remaining MCP::Server `t/21` flake with the project's
flaky-test protocol (`docs/flaky-test-policy.md`): the ledger may grade that one file through
`ecosystem/accepted-divergences.toml` only if the failure shape becomes deterministic, which
it is not today, so it stays a flaky-looking `partial`. Keep the pushed branch as the A/B
reference; do not merge it.

If the maintainer prefers (a) or (b), (b) is the sound one; (a) alone is a band-aid.

## 5. Rejected

Making the worker pool single-threaded for `start` bursts: removes real parallelism and breaks
timing-based programs in the other direction.

# ADR-17392: Under an address-space limit, user-code threads start small and grow their stack in segments

- **Status**: Accepted (user approval 2026-11-13)
- **Deciders**: tokuhirom, Claude
- **Context**: [#17392](https://github.com/tokuhirom/mutsu/issues/17392). Supersedes
  [ADR-0123](0123-bounded-user-thread-stack-reservations.md) D2 and its §6 amendment (the fixed
  stack tiers), and reverses the rejection of segmented stacks in
  [ADR-0100](0100-deep-recursion-raises-on-native-stack-headroom.md) §"Alternatives"
  ("Segmented / growable stacks (the `stacker` crate)"). ADR-0100's guard and ADR-0123's budget,
  D1, D3 and D4 stay.

## 1. Context

mutsu's `await` holds its OS thread: a Raku call is a Rust call, and there are no continuations.
So N `start` blocks parked in `await` need N threads, and each thread reserves a native stack
up front. Under `ulimit -v` (`RLIMIT_AS`) that reservation is real. ADR-0123 bounded it with a
budget (half the limit) and fixed tiers:

- an optional-growth worker took the full 256 MiB;
- a worker that had to exist stepped down through 64 and 32 MiB (and 16, 8 and 4 MiB in a
  release build, §6).

Under the ecosystem sandbox's 6 GB limit the first dozen workers spent the whole 3 GB budget at
256 MiB each, and every later one took the smallest tier. About 700 parked `start` blocks did not
fit in a release build, and about 150 in a debug build. Rakudo parks the same program on a
handful of threads.

The tiers also bound **depth** by accident of scheduling. A worker created under pressure got a
4 MiB stack and raised "Too deep recursion" after ~400 levels, where a worker created a moment
earlier could recurse 256 MiB deep.

What a parked thread actually uses is a few frames. What it has to reserve is the depth it
*might* reach. A growable stack makes those two the same thing.

## 2. Decision

### D1. A thread maps stack segments as deep as it recurses

A user-code thread may continue a call on a freshly mapped **stack segment** (64 MiB,
`native_stack::SEGMENT_BYTES`) when it runs below its floor (ADR-0100). It returns to the
previous stack when that call returns. The switch uses the `stacker` crate, which belongs to
rust-lang and is used by rustc for the same purpose.

The segments add up to the usable depth of the old 256 MiB stack
(`native_stack::LOGICAL_STACK_BYTES`) and no further. Each segment is reserved against the
ADR-0123 budget like a thread stack. So:

- a runaway recursion still ends in ADR-0100's catchable "Too deep recursion", at the same depth
  as on the 256 MiB main thread. It never ends in an out-of-memory abort, which was ADR-0100's
  objection to `stacker`.
- under a limit, a segment the budget cannot afford ends the recursion with the same catchable
  error, not with a failed `mmap`.

### D2. Every site that tests the floor first tries to continue on a segment

A segment switch has to wrap the rest of the call. So each site that checks the floor re-runs
**itself**, with the arguments it received, on the new segment (`grow_and_reenter!`). Only when
no segment is to be had does it fall through to its existing "Too deep recursion" handling. The
sites are:

- the ten VM / TRIR call boundaries (`guard_native_stack!`);
- the value renderers (`render_guard::render_on_fresh_segment`);
- the parser's two nesting points;
- the three RakuAST tree walks.

The call-boundary check stays amortized over `STACK_CHECK_INTERVAL` calls, so the hot path is
unchanged.

### D3. Under a budget, every user-code thread starts on the smallest guardable stack

`stack_budget::initial_stack_bytes`:

- **no address-space limit** (the normal case): 256 MiB, as before. No segment is ever needed,
  so nothing changes.
- **under a limit**: `MIN_GUARDABLE_STACK_BYTES` (2 MiB in a release build, 16 MiB in a debug
  build). This applies to every user-code thread, whether it was created by optional growth or
  because it had to exist. The thread grows by D1.

The smallest guardable stack is twice the smallest reserve. ADR-0123 §6 measured that reserve
as what covers one check interval's worst-case overshoot plus the unwind: 1 MiB in a release
build, 8 MiB in a debug one. A stack's reserve is therefore `min(16 MiB, stack / 4)` but never
less than that minimum. On the initial stack, the half above the reserve is only the headroom
before the first segment: depth comes from D1, so the initial stack sets only what a parked
worker costs.

The fixed tiers (`STACK_TIERS`) are gone. A parked worker costs the small stack, whichever
worker it is, and every worker may recurse equally deep.

The main thread keeps its 256 MiB stack and no segments.

## 3. Consequences

- Under the ecosystem sandbox's 6 GB limit, a release build parks 2000 `start` blocks in 0.6 s.
  ADR-0123 §6 managed about 700. A debug build parks 300, where it managed about 150. Past that,
  the thread that must exist is still created over the budget until the OS refuses it
  (ADR-0123 D3/D4). The cliff is now about `limit / 2 MiB`.
- **A task's depth no longer depends on which worker runs it.** Under a limit, deep recursion on
  a worker reaches the main thread's depth (budget permitting), where a stepped-down worker used
  to stop after ~400 levels.
- **Native recursion that no growable site sees is bounded by the current segment's reserve.**
  This covers a deep regex match and the drop of a deeply nested value. On a worker's initial
  stack under a limit, the reserve is 1 MiB in a release build. That is ADR-0123 §6's floor,
  already accepted for stepped-down workers; it now applies to every worker under a limit.
  Without a limit nothing changes.
- **A debug build fits an eighth as many parked workers as a release build.** Its 16 MiB floor
  is set by its frame size (ADR-0123 §6). The ecosystem sweep and `make test` run release
  builds.
- One more dependency (`stacker`, with `psm`: a build script and a little assembly per target).
  It is native-only; a build without segments (wasm32, no `native`) keeps the 256 MiB start.
- The parked thread still exists. This removes the address-space cliff, not the thread. At tens
  of thousands of parked tasks the next limit is the thread count (`ulimit -u`). Matching
  rakudo there needs heap-allocated frames (ADR-0100 §"Alternatives": moving call frames off the
  native stack), which this ADR does not decide.

## 4. Alternatives considered

- **A continuation-style `await`**: suspend the frame on the heap and release the worker. That
  is possible only for an `await` at the top of a `start` block's own frame. Inside a loop, a
  `try` or a nested call, the state lives in native Rust frames (`run_range` recursion,
  `catch_unwind`). The lazy `gather` coroutine shows the cost: per-construct resume state, and a
  soundness check that refuses nested calls. It would also need GC tracing for the suspended
  state, which `gather`'s does not have. That is several times the work for a narrower result.
- **Smaller fixed stacks for everyone** (ADR-0123 §4): moves the cliff and costs every program
  depth.
- **Heap-allocated call frames** (rakudo/MoarVM): the complete answer, and a full VM redesign.
  It is still not decided by this ADR; segments do not stand in its way.

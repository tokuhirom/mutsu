# ADR-0100: Deep recursion raises a catchable error, guarded by native stack headroom

- Status: Accepted (implemented)
- Date: 2026-09-13
- Issue: [#8232](https://github.com/tokuhirom/mutsu/issues/8232)

## Context

mutsu executes a Raku routine call as a **Rust** call: the VM's `exec_one`
dispatch frame, the call-op handler, signature binding, and — for a re-entrant
construct — a whole `eval_block_value` frame all sit on the native stack, and
the callee's body loop runs nested inside them. Raku recursion is therefore
native recursion.

When the native stack runs out, the thread hits its guard page and the Rust
runtime **aborts the process**. There is no unwinding, so:

- `try` / `CATCH` cannot contain it,
- `END` phasers do not run,
- there is no backtrace and no exit code a script can act on,
- and the rest of a test file after the offending statement never runs.

```raku
my $d = 0;
sub rec($n) { $d = $n; rec($n+1) unless $n >= 20000 }
END { note "depth reached: $d" }
rec(1);
```

rakudo prints `depth reached: 20000`; mutsu printed
`fatal runtime error: stack overflow, aborting` and nothing else.

This is not only a diagnostics problem. Any program whose input nesting depth
is attacker-controlled — a JSON or XML document, a recursive-descent parser
over user text — turns a deeply nested input into an unconditional process
abort that no amount of defensive `try` can contain. `JSON::Fast`'s
`t/01-parse.t` feeds its parser `Q«[{"":» x 10_000` (≈20,000 Raku frames)
precisely to check that this is survivable.

rakudo has no fixed recursion limit: it grows the call stack on the **heap**
and eventually fails with an ordinary, catchable out-of-memory exception.
mutsu cannot copy that without moving call frames off the Rust stack, which is
a VM redesign (see "Rejected alternatives").

## Decision

**Guard on remaining native stack headroom at the VM's call boundary, and
raise an ordinary catchable Raku exception when it is exhausted.**

1. **The measurement is the stack, not a frame count.** Each thread that runs
   user VM code records, at start-up, the lowest address it may safely touch
   (`stack top − stack size + reserve`). At a call boundary the VM takes the
   address of a stack local and compares. That is one load, one subtract and
   one branch, with no counter to keep coherent across the several call paths
   and no state to unwind.

2. **The chokepoints are the VM's call entry points.** The seven functions
   that carry a `SafepointKind::Call` GC safepoint or push a call frame —
   `call_compiled_function_fast`, the two light paths,
   `call_compiled_function_named_inner`, `call_compiled_closure_in_unit`, and
   the two `call_compiled_method*` entries. All seven already return
   `Result<_, RuntimeError>`, and all seven are already established as points
   that hold no container borrow, which is what a raise needs. Between them
   they cover every shape of Raku recursion: sub, method, closure/block, and
   both JIT-on and JIT-off dispatch.

3. **The check is amortized over 32 calls.** Reading the stack pointer needs
   the address of a local, which forces a stack slot and acts as an
   optimization barrier in the hottest functions mutsu has. The hot path is
   therefore a countdown on an `Interpreter` field, and the real read happens
   once per `STACK_CHECK_INTERVAL` calls. The reserve absorbs the overshoot.

4. **The reserve is 16 MiB of a 256 MiB stack (6%).** The guard fires while
   there is still room for three things: an interval's worth of overshoot
   (32 frames of the fattest kind measured — ~156 KiB each, an interpreted
   non-JIT frame in a debug build — is ~5 MiB), the native recursion a
   *single* Raku call can perform between two checks (a deep regex match,
   dropping a deeply nested value), and then constructing the error,
   unwinding, and running the `END` phasers the unwind reaches.

5. **The error is `X::AdHoc`**, message
   `Too deep recursion (out of stack space)`. rakudo has no dedicated
   `X::Recursion` to match, and inventing a mutsu-only type would make the
   obvious `CATCH { when X::Whatever { } }` miss it. `X::AdHoc` is what every
   generic handler already catches, and the message is what a user sees.

6. **Not configurable.** The knob that matters is the stack size itself, and
   it is already uniform (below). A recursion-depth setting would be a promise
   mutsu cannot keep, because the per-call stack cost is not a constant: a
   plain compiled call is cheap and a re-entrant `eval_block_value` is not, so
   the same "depth" buys different amounts of stack in different programs.

7. **Every thread that runs user VM code already has the same 256 MiB stack**,
   so the guard fires at comparable depth everywhere. `main.rs` spawns
   `mutsu-main` with `stack_size(256 MiB)` and
   `builtins_system::USER_THREAD_STACK_SIZE` (the `start` / Promise / Supply
   worker stack) is the same 256 MiB constant. The two
   `stack_size(16 * 1024 * 1024)` spawns in `src/runtime/mod.rs` are
   `#[test]` helpers, not user-visible threads — #8232 read them as a 16×
   lower ceiling for user code, and that part of the report is wrong. Service
   threads (timer, socket pump, signal reader) keep the default stack because
   they run no user VM code.

## Consequences

- Deep recursion now raises where it used to abort. `try`, `CATCH`, `END` and
  a normal exit status all work, and a test file continues past the failure.
- **It costs about 3% on a call-only microbenchmark.** Local A/B on this
  container measured `bench-fib` and `method-call` 2-4% slower and `bench-tak`
  flat. A control build with the guard's *body* replaced by `Ok(())`, call
  sites unchanged, measured the same delta, and two independently built
  baselines measured the same as each other — so the cost is the per-call-site
  overhead in those functions, not the countdown arithmetic, and there is no
  cheaper arrangement of the same check. The authoritative numbers are the
  bench CI's (`bench-history.tsv` on `bench-data`), not these. Paying it buys
  the removal of an *uncatchable process abort*, which by the repository's own
  gain/risk definition is the trade to take.
- **The depth at which it raises is a property of the build, not of the
  language.** A debug frame is several times larger than a release one, so a
  debug run raises earlier. Nothing may assert a specific depth; the
  regression pin (`t/vm/deep-recursion-raises-instead-of-aborting.t`) asserts
  only that the failure is *catchable* and that execution continues. Measured
  for scale: ~5,700 frames on a debug build with JIT on, ~1,500 without.
- mutsu still raises where rakudo would keep going. The pure-Raku repro above
  reaches its 20,000 under rakudo and raises earlier under a debug mutsu. That
  is a real remaining compatibility gap, and the lever for it is reducing the
  per-call native stack cost — a separate, worthwhile perf/architecture
  question that this ADR does not settle and does not depend on.
- **Known gap: the guard is inert where mutsu does not own the thread.** An
  `Interpreter` driven from a `#[test]`, or from the wasm library build, never
  runs the per-thread initialisation, so the limit stays zero and the check is
  skipped — today's behaviour, unchanged. Those threads have no stack bound
  mutsu can discover portably (`pthread_getattr_np` is glibc-only and `libc`
  is an optional dependency here). The initialisation entry point is public,
  so an embedder that knows its own stack size can opt in.

## Rejected alternatives

**A bigger stack.** Moves the cliff without removing it, and cannot be sized
correctly for the reason in point 6: the per-call cost depends on the
construct. It also does nothing about the failure *mode*, which is the actual
complaint — an abort is uncatchable at any stack size.

**A fixed recursion-depth counter.** Deterministic across builds, which is its
one real advantage, but it cannot be both safe and generous. It must be sized
for the worst case (a debug build, whose frames are the largest), which then
makes it needlessly strict for release, and it would still miss native
recursion that is not a routine call at all — a deeply nested regex, or
dropping a deeply nested value. A counter also has to be kept coherent across
five call paths and every unwind, where the stack pointer needs no maintenance
because it *is* the state.

**Moving call frames off the native stack** (rakudo/MoarVM's model: an
explicit heap-allocated frame chain, so depth is bounded by memory). This is
the only design that would match rakudo's behaviour rather than approximate
it, and it is a full VM redesign — the same class of change ADR-0001 §"level 2"
rejects without a measured ceiling forcing it. The guard here is not an
obstacle to it: if frames ever move to the heap, the headroom check simply
stops firing.

**Segmented / growable stacks (the `stacker` crate).** Allocates a fresh stack
segment when headroom runs low, so recursion is bounded by memory rather than
by the initial stack. It removes the limit rather than reporting it, which is
attractive, but it adds an allocation on a hot boundary, interacts with the
GC's notion of thread stacks, and — because the depth is then bounded by
memory — turns the failure back into an OOM whose catchability is exactly the
thing being fixed. Reconsider only if the compatibility gap above is measured
to matter.

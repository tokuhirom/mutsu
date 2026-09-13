# Deep recursion raises instead of aborting the process

```raku
my $d = 0;
sub rec($n) { $d = $n; rec($n+1) unless $n >= 20000 }
END { note "depth reached: $d" }
rec(1);
```

used to print

```
thread 'mutsu-main' has overflowed its stack
fatal runtime error: stack overflow, aborting
```

and nothing else — no `END` phaser, no backtrace, no exit status a script could
act on, and nothing for `try` or `CATCH` to contain. It now prints
`depth reached: 5751` and raises an ordinary catchable exception.

## Why it aborted

mutsu executes a Raku call as a **Rust** call: the `exec_one` dispatch frame,
the call-op handler, signature binding, and for a re-entrant construct a whole
`eval_block_value` frame all sit on the native stack, with the callee's body
loop running nested inside them. Raku recursion is native recursion, and when
the native stack runs out the thread hits its guard page and the Rust runtime
aborts. There is no unwinding, so no amount of defensive `try` helps — which
matters most for a program whose input depth is attacker-controlled, where one
deeply nested document is an unconditional process kill.

The 256 MB stack `main.rs` already asks for was never the issue: it moves the
cliff without removing it.

## The guard

[ADR-0100](../../docs/adr/0100-deep-recursion-raises-on-native-stack-headroom.md)
records the decision. Each thread that runs user VM code notes, at start-up,
the lowest stack address it may still make a call from (its stack top, minus
its size, plus a 16 MiB reserve). The VM's seven call entry points — the ones
that already carry a `SafepointKind::Call` GC safepoint or push a call frame —
compare against it and raise `X::AdHoc` with
`Too deep recursion (out of stack space)` rather than proceed. That covers sub,
method, closure and block recursion, with JIT on or off.

The measurement is the stack pointer rather than a frame count on purpose: the
per-call cost is not a constant (measured at ~42 KiB for a JIT-on debug frame
and ~156 KiB for an interpreted one), so a depth limit generous enough for one
program would be unsafe for another. The stack pointer needs no maintenance
because it *is* the state. Reading it at every call cost ~3% on a call-only
microbenchmark, so the hot path is a countdown on an `Interpreter` field and
the read happens once per 32 calls; the reserve is sized to absorb the
overshoot.

The depth at which it fires is therefore a property of the build, not of the
language, and `t/vm/deep-recursion-raises-instead-of-aborting.t` asserts only
what is invariant: the failure is catchable, a `try` contains it, and execution
continues afterwards.

## What is still different from rakudo

rakudo has no recursion limit — it grows its call stack on the heap and fails
with an ordinary out-of-memory exception — so it reaches the 20,000 frames
above where mutsu raises earlier. Closing that gap means reducing the per-call
native stack cost, or moving frames off the native stack entirely; the ADR
records both as separate questions, and the guard is not an obstacle to either.

Two corrections to what [#8232](https://github.com/tokuhirom/mutsu/issues/8232)
recorded, established while implementing this: the two 16 MB
`stack_size` spawns it names in `src/runtime/mod.rs` are `#[test]` helpers, not
user-visible threads — `start`/Promise/Supply workers already get the same
256 MB stack as `mutsu-main` (`builtins_system::USER_THREAD_STACK_SIZE`), so
there was no 16× lower ceiling for threaded user code to audit.

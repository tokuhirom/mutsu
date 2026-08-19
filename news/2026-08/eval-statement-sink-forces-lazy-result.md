# A bare `EVAL '...';` statement now forces a deferred `gather`/lazy-IO-lines result

A bare call statement sinks its return value, and `OpCode::SinkPop` already
knew to *force* a deferred `LazyList` (e.g. a `gather` block) or
`LazyIoLines` iterator when sinking one — matching raku, which reifies a
lazy result at the point it is discarded. But `EVAL` compiles to a different,
"statement-level call, no return value kept" bytecode form
(`OpCode::ExecCall`/`ExecCallPairs`) that never reaches `SinkPop` at all, so
`EVAL 'gather { ... }';` as a bare statement silently never ran the gather
body:

```raku
EVAL 'gather { print "ran "; take 1 }';
say "after";
```

raku prints `ran after`; mutsu printed only `after` — the body never ran.

This matters beyond a synthetic repro: the real, vendored `Test.rakumod`'s
`throws-like` runs `EVAL $code, context => $ctx;` as exactly this kind of
bare, named-arg statement call (a mid-body statement, not the block's tail
value), so `throws-like 'gather { return 1 }', X::ControlFlow::Return` never
even entered the gather body under `MUTSU_REAL_TEST=1` —
`t/throws-like-gather-sink.t`'s first subtest in the ongoing
`todo/deep/vendor-real-test-module.md` campaign.

Fixed by adding the same `LazyList`/`LazyIoLines`-forcing match arms
`SinkPop` already has to `sink_discarded_call_value`
(`src/vm/vm_call_exec_ops.rs`), the shared helper both `ExecCall` and
`ExecCallPairs` already used for the Failure-exploding half of sink
semantics. Pin: `t/eval-statement-sinks-lazy-result.t`, verified against
`raku`.

This closes one of the four subtests in `t/throws-like-gather-sink.t`; the
other three ("a bare `return`/`return` in a `for` loop at mainline throws
X::ControlFlow::Return" under a `context =>`-scoped `EVAL`) need the deeper
"`EVAL`'s `context` argument must carry the *frame* a `return` should target,
not just the package" mechanism described in
`todo/deep/eval-context-frame-owns-the-return-target.md`, left open.

# `...` stub code uses `fail` semantics (returns Failure to caller)

Raku's `...` (stub code / `WhateverCode` placeholder) uses `fail()` internally,
not `die()`. This means calling a stub routine returns a `Failure` to its caller
rather than immediately throwing an exception.

Previously mutsu used `die` semantics for `...`, which caused `eval-lives-ok`
in `roast/integration/advent2009-day20.t` (tests 12–13) to fail. The relevant
code path is:

```raku
sub eval_exception($code) {
    try { EVAL ($code) }  # EVAL returns lazy Seq; try exits OK
    $!                    # $! is Any (no exception caught)
}
```

When `EVAL q[map -> $x, $y { ... }, 1..6]` returns a lazy `Seq`, that Seq is
sunk by `SinkPop` **outside** the `try` scope. With the old `die` semantics, the
exception from the stub call escaped the sub boundary as a thrown exception,
crashing the caller. With `fail` semantics, the `is_fail` signal at the sub
boundary is converted to a `Failure` value, so the caller receives
`Failure(X::StubCode)` — which is `not defined` — and `eval-lives-ok` passes.

Existing `try { ... }` behaviour is unchanged: `try` catches `fail` signals the
same way it catches `die`, so `$! ~~ X::StubCode` after `try { ... }` still
holds. Stub class methods now correctly return `Failure` to their callers instead
of throwing.

Pinned by `t/stub-fail-semantics.t`.

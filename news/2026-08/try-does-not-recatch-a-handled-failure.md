# A `try` does not re-catch a `Failure` something already handled

A `try` whose body yields a *live* `Failure` catches it: the Failure becomes
handled and `$!` holds its exception. mutsu did that for **any** trailing
Failure, including one that `.defined` / `.Bool` / `.so` had already handled —
and by then it is an ordinary value with nothing left to catch:

```
$ raku  -e 'my $f = "foo"[2]; $f.defined; try { $f }; say (defined $!) ?? "died" !! "lived"'
lived
$ mutsu -e 'my $f = "foo"[2]; $f.defined; try { $f }; say (defined $!) ?? "died" !! "lived"'
died
```

No `EVAL` and no module are involved; `"foo"[2]` produces the `Failure`
directly. `TryCatch`'s success path now skips a Failure whose handled flag is
already set, so `$!` stays undefined.

The unhandled case is unchanged — `try { "foo"[2] }` still sets `$!` to the
`X::OutOfRange`, which is what raku does too.

## Why it mattered

rakudo's `Test.rakumod` writes `lives-ok` as

```raku
multi sub lives-ok(Callable $code, $reason = '') is export {
    try { $code(); }
    my $ok = proclaim((not defined $!), $reason) or _diag($!);
    ...
}
```

so any `lives-ok { ... }` whose block ends in a handled `Failure` was reported as
*died*. Found through the Test-vendoring sweep
(`todo/tickets/vendor-real-test-module.md`).

## How it was tracked down

The tell was that `$f.handled` read `True` both outside and *inside* the `try`,
so the handled flag was being set correctly and something was ignoring it. A
breakpoint on `Value::is_failure_handled` showed it consulted exactly once, with
the right instance id, returning `true` — so `OpCode::ThrowIfFailure` was not the
thrower at all, which is what an `eprintln!` at the throw site would never have
revealed. That left the `TryCatch` arm, which converts a trailing Failure to `$!`
without asking.

Pinned by `t/try-does-not-recatch-a-handled-failure.t`, whose 7 assertions are
green under `raku` too.

## What it did not fix

`t/statement-call-sinks-its-value.t` is still red under the aliased module,
because a `throws-like` immediately before a `lives-ok` leaves `$!` set where the
`lives-ok`'s own `try` does not reset it —
`todo/tickets/error-var-leaks-past-a-later-try.md`.

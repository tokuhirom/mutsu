# `$!` set by one test assertion is still visible to the next one's `try`

Under rakudo's real `Test.rakumod`, a `throws-like` immediately before a
`lives-ok` makes the `lives-ok` report *died*:

```
$ cat tmp/lives-probe2.raku
use Test2;
use MONKEY-SEE-NO-EVAL;
plan 3;
throws-like { EVAL q{use fatal; "foo"[2]}; }, X::OutOfRange, 'a';
lives-ok { my $x := EVAL q{"foo"[2]}; $x.defined }, 'b';
lives-ok { my $f = EVAL q{"foo"[2]}; $f.defined; $f }, 'c';

$ raku  -I tmp/core tmp/lives-probe2.raku      # ok 1, ok 2, ok 3
$ mutsu -I tmp/core tmp/lives-probe2.raku      # ok 1, not ok 2, not ok 3
#   # Index out of range. Is: 2, should be in 0..0
```

Drop the `throws-like` and both `lives-ok`s pass. So the assertion that *did*
throw leaves `$!` set somewhere the next assertion's `try` does not reset.

`lives-ok` is

```raku
multi sub lives-ok(Callable $code, $reason = '') is export {
    try { $code(); }
    my $ok = proclaim((not defined $!), $reason) or _diag($!);
    ...
}
```

so a stale `$!` is read as "the code died". mutsu's `TryCatch` success path does
write `!` (to `Any`) in its own env, and that is enough when the preceding
assertion did not throw — the write simply does not reach whatever scope
`throws-like`'s `CATCH` wrote to.

This is the `$!`-scoping family of `news/2026-08/...` / #5420 ("`$!` disappears
across a call" — the routine-return env merge used to drop `!`), but the failing
direction is the opposite one: a write that should be shadowed is instead seen
by a later frame.

Found while fixing `news/2026-08/try-does-not-recatch-a-handled-failure.md`,
which removed the *other* reason those two assertions failed (a `try` re-catching
an already-handled Failure). It is what still keeps
`t/statement-call-sinks-its-value.t` red under the aliased upstream module —
assertions 4 and 8, both `lives-ok` immediately following a `throws-like`.

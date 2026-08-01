# `use fatal` inside a string-form `throws-like` does not take effect

Found while re-running the Test-vendoring sweep
(`todo/tickets/vendor-real-test-module.md`). Under rakudo's real
`Test.rakumod`, the string form of `throws-like` reports that the code did not
die at all:

```
$ cat tmp/fatal-probe.raku
use Test2;
plan 2;
throws-like 'use fatal; "foo"[2]', X::OutOfRange, 'string form';
throws-like { use fatal; "foo"[2] }, X::OutOfRange, 'block form';

$ mutsu -I tmp/core tmp/fatal-probe.raku
    not ok 1 - 'use fatal; "foo"[2]' died      # raku: ok
ok 2 - block form                              # the block form is fine
```

`"foo"[2]` returns a `Failure`; `use fatal` is what turns it into a throw. The
block form works, so `use fatal` itself is not the problem — the string form is.

This is the last of the seven `t/` files that regressed under the real module on
an exception-classing signature; the other six were fixed by
`news/2026-08/typed-exception-class-from-the-message-convention.md` and
`news/2026-08/parse-failures-carry-a-syntax-exception-class.md`. It is the only
one that is not about the *class* of the exception — nothing is thrown.

## What has been ruled out

`Test.rakumod`'s string branch is

```raku
EVAL $code, context => $caller-context;
```

inside the `subtest { ... }` block, with `$caller-context` from
`$*THROWS-LIKE-CONTEXT // CALLER::`. Each of these narrower shapes **does**
throw correctly, so none of them is the cause on its own:

- `try { EVAL $c }` in the mainline
- `sub f($code) { EVAL $code }` — plain, from a sub
- `sub f($code) { my $ctx = CALLER::; EVAL $code, context => $ctx }` — with an
  explicit context
- the same two shapes exported from a module (`tmp/core/FatalMod.rakumod`)

So the loss needs the remaining ingredient the real module adds: the `EVAL` runs
inside a `subtest` block that also carries a `CATCH { default { ... } }`. The
next step is to reproduce it with a hand-written `subtest`-plus-`CATCH` wrapper
and find where the pragma stops being applied to the EVAL'd unit — most likely a
place where the enclosing block's compilation is re-entered and the `fatal`
pragma is read from the wrong unit.

Affected test file: `t/out-of-range-scalar-index.t` (its first two assertions).

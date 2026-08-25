# `X::Proc::Async::CharsOrBytes.Str` gives the message, not the type name

```raku
my $proc = Proc::Async.new('echo');
$proc.stdout.tap(&print);
$proc.stdout(:bin).tap(&print);
CATCH { default { put .^name, ': ', .Str } };
```

- rakudo: `X::Proc::Async::CharsOrBytes: Can only tap one of chars or bytes supply for stdout`
- mutsu (before): `X::Proc::Async::CharsOrBytes: X::Proc::Async::CharsOrBytes`

The exception type and the `handle` attribute were already right; only the
rendering was wrong, and it was wrong for the *whole* `X::Proc::Async::*` family
for one shared reason: the `proc_async_error` builder stored the bare class name
in the exception's `message` attribute, which shadows the
`format_exception_message()` table that `.message` / `.Str` / `.gist` otherwise
consult.

The fix is described in
[`x-proc-async-mustbestarted-str-not-message.md`](x-proc-async-mustbestarted-str-not-message.md),
which this divergence shares end to end: the builder now stores only the real
Raku attributes and lets the formatter produce the text, and
`format_exception_message()` grew an arm per type —
`CharsOrBytes` renders `Can only tap one of chars or bytes supply for {handle}`.

Pinned by `t/proc-async-divergences.t`, which asserts `.^name`, `.message` and
`.Str` for this type (and checks `.Str` equals `.message`, the exact shape that
was broken).

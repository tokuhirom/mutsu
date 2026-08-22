# `X::Proc::Async::CharsOrBytes.Str` returns the type name instead of the exception message

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/X/Proc/Async/CharsOrBytes.rakudoc:14`).

## Root cause hypothesis

When tapping both `.stdout` and `.stdout(:bin)` on the same `Proc::Async`, mutsu
correctly throws an `X::Proc::Async::CharsOrBytes` exception (the `.^name` in the
harness output matches raku exactly), but the exception's `.Str` (and presumably
`.message`) returns the literal type name `X::Proc::Async::CharsOrBytes` instead of the
descriptive message ("Can only tap one of chars or bytes supply for stdout"). This
looks like `X::Proc::Async::CharsOrBytes` is missing its `.message` method override (or
the constructor doesn't populate whatever field `.message`/`.Str` reads), so it falls
back to the generic default that just stringifies the type.

## Minimal repro

```raku
my $proc = Proc::Async.new('echo');
$proc.stdout.tap(&print);
$proc.stdout(:bin).tap(&print);
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::Proc::Async::CharsOrBytes: Can only tap one of chars or bytes supply for stdout`
- `mutsu` (`target/debug/mutsu`): `X::Proc::Async::CharsOrBytes: X::Proc::Async::CharsOrBytes`

## Affected files (starting point)

Wherever `X::Proc::Async::CharsOrBytes` is thrown/constructed (grep for
`"CharsOrBytes"` in `runtime/`) — needs a `.message` method that builds the descriptive
string (it should mention which of stdout/stderr and note "chars or bytes"), matching
the pattern used by other `X::*` exception types that already have a working
`.message`.

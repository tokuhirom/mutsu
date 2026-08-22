# `X::Proc::Async::MustBeStarted.Str` returns the class name instead of its message

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/X/Proc/Async/MustBeStarted.rakudoc:14`).

## Root cause hypothesis

`X::Proc::Async::MustBeStarted` is thrown when a method requiring a started `Proc::Async` (e.g.
`.say`, `.print`, `.write`) is called before `.start`. Its `.Str`/message rendering should produce
`"Process must be started first before calling '<method>'"` (interpolating the offending method
name), but mutsu's `.Str` for this exception just returns the bare type name
`"X::Proc::Async::MustBeStarted"` instead — i.e. the exception's `message`/`Str` method either
isn't overridden for this type, or is falling back to a generic default that stringifies to the
class name.

## Minimal repro

```raku
Proc::Async.new('echo', :w).say(42);
CATCH { default { put .^name, ': ', .Str } };
```
- `raku`: `X::Proc::Async::MustBeStarted: Process must be started first before calling 'say'`
- `mutsu`: `X::Proc::Async::MustBeStarted: X::Proc::Async::MustBeStarted`

## Affected files (starting point)

- Wherever `X::Proc::Async::MustBeStarted` is defined/thrown (search for `MustBeStarted` in
  `src/runtime/`) — needs a `.message`/`.Str` (or the general exception-message-formatting hook
  other `X::` types use) that interpolates the method name that triggered it, not just the bare
  class name.

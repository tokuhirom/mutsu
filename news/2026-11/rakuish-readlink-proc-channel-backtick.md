# Rakuish: IO::Path.readlink, Proc::Async channels, backticks

Running Rakuish's suite exposed these differences from rakudo, all fixed:

- `IO::Path.readlink` exists. It answers a symlink's target, made absolute
  against `$*CWD`, with the link's directory (or a given `CWD`) as its `CWD`.
  A path that is not a symlink answers itself, and a missing one is the
  `X::IO::DoesNotExist` Failure of `.l`. Rakuish's sandbox walks symlink
  chains with it.
- `$proc.stdout.Channel`, taken before `.start`, now taps the stream live.
  Before, it snapshotted an empty Supply and closed the channel at once, so
  Rakuish's `exec` captured no output.
- A `.method` written inside a `#` comment no longer makes a `{ k => v, … }`
  hash composer a block.
- A backtick is no quoting construct: `` `echo pwned` `` is a compile error,
  as in rakudo, instead of running a shell. Rakuish's REPL relies on that
  rejection to refuse shell captures.

The suite also exposed a thread shared-store bug: a module routine's
`my Int $n` is reset by an `await` when the script has a `$n` of its own. It
is filed as #17747.

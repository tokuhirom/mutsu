# `open()` doesn't recognize an `IO::Special` object (`<STDOUT>`/`<STDERR>`/`<STDIN>`) as a special-handle target

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/newline.rakudoc:40`).

## Root cause hypothesis

`IO::Special.new('<STDOUT>')` (and `<STDERR>`/`<STDIN>`) is a sentinel object that, when passed
to `open()`, is supposed to return a handle wrapping the corresponding standard stream (so you
can reopen it with different options, e.g. a custom `:nl-out`):

```raku
my $crlf-out = open(IO::Special.new('<STDOUT>'), :nl-out("\\\n\r"));
$*OUT.say: 1;     # 1
$crlf-out.say: 1; # 1\  (with a literal backslash + CRLF newline)
```

mutsu's `open()` instead treats the `IO::Special` argument like an ordinary path/filename and
tries to `open()` it as a literal file, which of course fails:

```
Failed to open '.../IO::Special()': No such file or directory (os error 2)
```

(Note the error text itself — `'.../IO::Special()'` — shows the object's stringification/gist
being concatenated onto the CWD as if it were a relative path, confirming `open()` never special-cased the `IO::Special` type at all.)

## Minimal repro

```raku
my $crlf-out = open(IO::Special.new('<STDOUT>'), :nl-out("\\\n\r"));
$*OUT.say: 1;
$crlf-out.say: 1;
```

- `raku` stdout (raw bytes): `1\n1\\\n\r` (i.e. `1`, newline, `1\`, then a literal `\r`)
- `mutsu` (`target/debug/mutsu`): prints `1` for the first `.say`, then dies:
  ```
  Failed to open '<cwd>/IO::Special()': No such file or directory (os error 2)
  ```

## Affected files (starting point)

- `open()`'s implementation — likely `src/runtime/builtins_*.rs` (file/IO builtins) or wherever
  `IO::Path`/`open` argument coercion happens — needs to check whether the first argument is an
  `IO::Special` instance (holding one of `<STDOUT>`/`<STDERR>`/`<STDIN>`) and dispatch to
  wrapping the corresponding already-open standard stream (applying any given `:nl-out`/other
  adverbs) instead of falling through to the ordinary path-based `open()` logic.

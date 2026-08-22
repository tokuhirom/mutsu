# `lines(IO_object)` sub form doesn't open/read the file — it wraps the arg unchanged

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Type/IO/Handle.rakudoc:385`
and `Type/independent-routines.rakudoc`'s `lines` section).

## Root cause hypothesis

The global `lines()` routine (sub form, as opposed to the `.lines` method) does not
dispatch to the same file-reading logic as the method form. When called with an
`IO::Path` positional argument, it appears to just wrap the argument itself into a
one-element list instead of opening the path and reading its lines.

The **method** form (`"path".IO.lines`) works correctly — only the **sub** form
(`lines($io-path)`) is broken.

## Minimal repro

```raku
my $io = "tmp/lines-test.txt".IO;  # file contains "a\nb\nc\n"
say lines($io);          # sub form
say $io.lines;           # method form
```

- `raku`: `(a b c)` for both.
- `mutsu` (`target/debug/mutsu`): sub form prints `(tmp/lines-test.txt)` (the
  `IO::Path`'s stringified path wrapped in a list) instead of reading the file; the
  method form correctly prints `(a b c)`.

Also reproduces with `/proc/$*PID/statm` (the doc's own example) — mutsu prints
`(/proc/<pid>/statm)` instead of the file's numeric fields (raku's own numbers are of
course PID/environment-dependent, but the *shape* of mutsu's wrong output — a
1-element list containing the literal path string — is not).

## Affected files (starting point)

- Wherever the global `lines` sub is dispatched (likely `src/runtime/builtins*.rs` or
  `src/runtime/builtins_io.rs`) — needs to detect an `IO::Path`/`IO::Handle` positional
  argument and delegate to the same open+read-lines logic the `.lines` method uses,
  rather than treating the argument as an opaque value to wrap.

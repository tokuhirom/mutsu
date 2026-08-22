# `$*ARGFILES.eof`/`.get` loops forever instead of terminating when input is exhausted

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/IO/ArgFiles.rakudoc:34`).

## Root cause hypothesis

`$*ARGFILES` (the magic "all `@*ARGS` files, or stdin if none given" handle) should
report `.eof` as `True` once its underlying source is exhausted, so a
`while ! $*ARGFILES.eof { say $*ARGFILES.get }` loop terminates. mutsu's `.eof` never
flips to `True` after the source is exhausted (or `.get` doesn't advance/signal
exhaustion the way `.eof` checks for), so the loop spins forever, printing `Nil` on
every iteration (`.get` on an exhausted stream correctly returns `Nil`, but the loop
condition never sees that as "done").

## Minimal repro

```raku
while ! $*ARGFILES.eof {
    say $*ARGFILES.get;
}
```

Run with no file arguments and closed/empty stdin (`< /dev/null`):

- `raku`: terminates immediately (prints at most one `Nil` depending on how the
  zero-file/`<>`-over-stdin case is modeled, then exits 0).
- `mutsu` (`target/debug/mutsu`): never terminates — an infinite loop printing `Nil`
  forever (times out under `timeout N`, exit 124).

## Affected files (starting point)

- Wherever `$*ARGFILES`/`IO::ArgFiles` is implemented (grep for `ARGFILES` in
  `src/runtime/`) — check the `.eof` implementation's interaction with the
  zero-files/stdin-fallback case specifically; it likely works correctly for the
  "at least one real file argument" case (since that path isn't flagged here) but
  mishandles the "no file args, read from stdin" fallback.

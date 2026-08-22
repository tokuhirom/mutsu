# `Formatter.new(FORMAT_STRING)` is unimplemented

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/Formatter.rakudoc:16,32`).

## Root cause

`Formatter` (a `v6.e.PREVIEW` type) compiles an `sprintf`-style format string into a
reusable `Callable` — `Formatter.new("'%5s'")` returns a `&handle` sub that formats its
argument the same way `sprintf("'%5s'", $arg)` would. mutsu already has a `Formatter`
type object stub (`say Formatter.^name` prints `Formatter`), but no `.new` method is
registered for it, so any construction attempt throws.

## Minimal repro

```raku
use v6.e.PREVIEW;
my &handle = Formatter.new("'%5s'");
say handle("foo");
```

- `raku`: `'  foo'`
- `mutsu` (`target/debug/mutsu`):
  ```
  X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on Formatter
  ```

A second example from the same doc (`Formatter.new("%05d")` used as `zero5(42)` →
`00042`) fails the same way.

## Affected files (starting point)

- Wherever the `Formatter` type object is registered (grep for `"Formatter"` in
  `src/runtime/runtime_init.rs` or similar) — needs a `.new` implementation that
  parses the format string once (reusing mutsu's existing `sprintf`/`printf`
  directive-parsing logic, since the format-string grammar is identical) and returns a
  `Sub`/`Callable` closing over the parsed format spec.

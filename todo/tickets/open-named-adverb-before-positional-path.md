# `open(:w, PATH)` — a named adverb before the positional path argument breaks argument parsing

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:473`).

## Root cause

`builtin_open` (`src/runtime/builtins_io.rs::builtin_open`, ~line 376) unconditionally
treats `args.first()` (the literal first element of the args slice) as the path, and
`args[1..]` as the mode flags:

```rust
let path = match args.first() {
    Some(v) => match v.view() { ... _ => v.to_string_value() },
    None => return Err(...),
};
...
let (read, write, ...) = self.parse_io_flags_values(&args[1..]);
```

This assumes the path is always the syntactically-first argument. But raku call
syntax allows a named argument to appear *before* a positional one at the call site
(`open :w, '/path'` is equivalent to `open '/path', :w`) — when that happens, `args[0]`
is actually the `:w` Pair (`"w" => True`), not the path, and mutsu ends up trying to
open a file literally named after the stringified Pair.

## Minimal repro

```raku
my $fh = open :w, "/tmp/mutsu-open-test.txt";
$fh.say: "hi";
$fh.close;
```

- `raku`: works, creates/writes the file.
- `mutsu` (`target/debug/mutsu`): dies with
  `Failed to open '.../w\tTrue': No such file or directory (os error 2)` — the `:w`
  Pair got stringified (`"w\tTrue"`, tab-separated) and treated as the filename.

Also affects the doc's own example (`open :w, '/tmp/some-file.txt'`).

## Affected files (starting point)

- `src/runtime/builtins_io.rs::builtin_open` (~line 376-395) — should scan `args` for
  the first *non-Pair* (or otherwise-not-a-named-adverb) positional value to use as
  the path, and treat every `Pair` argument (regardless of position) as a flag, the
  way `parse_io_flags_values` presumably already does for the flags themselves.
- `src/runtime/native_io/io_handle.rs`'s `"open"` method arm may have a similar
  positional-vs-named ordering assumption worth checking (`IO::Handle.new(...).open(:w
  ...)`  uses only named args so is less likely affected, but worth a quick check).

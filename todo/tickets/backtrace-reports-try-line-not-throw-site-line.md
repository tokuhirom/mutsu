# `$!.backtrace` after a `try` block reports the `try` statement's own line, not the actual `die`'s line

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/X/AdHoc.rakudoc:20`).

## Root cause hypothesis

When an exception is caught by `try` (or a `CATCH` block) and its `.backtrace` is read
afterward, mutsu's backtrace records the source line of the `try {` statement itself
rather than the line the `die`/throwing expression actually executed on. Real Rakudo's
backtrace always points at the throw site, regardless of how many lines the `try`
block's body spans before the `die`.

## Minimal repro

```raku
try {
    die "boom";
}
say $!.backtrace.Str;
```

(the `die` is on line 3, inside a `try` starting on line 2)

- `raku`: `  in block <unit> at -e line 3`  (correctly points at the `die`)
- `mutsu` (`target/debug/mutsu`): `  in block <unit> at -e line 2`  (points at the
  `try` line instead)

A `die` **not** inside a `try` (bare top-level) already reports the correct line —
this only reproduces for the backtrace attached to an exception object recovered via
`$!` after a `try`/`CATCH`.

## Affected files (starting point)

Wherever the `try`/`CATCH` VM op constructs or attaches the exception's backtrace
(`vm/vm_control_ops.rs` try/catch handling) — likely capturing the current line number
at the wrong point (the `try` statement's compiled position) instead of propagating the
line recorded at the actual `die`/throw call site.

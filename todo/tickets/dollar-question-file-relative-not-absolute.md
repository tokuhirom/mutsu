# `$?FILE` reports the relative invocation path instead of an absolute path

Discovered via the doc-diff harness on `raku-doc/doc/Language/variables.rakudoc` (around line
318).

## Repro

```
say "$?FILE: $?LINE";
```

Run as `raku tmp/t1.raku` vs. `target/debug/mutsu tmp/t1.raku` from the same working directory:

- raku: `/absolute/path/to/tmp/t1.raku: 1`
- mutsu: `tmp/t1.raku: 1`

`$?FILE` is a compile-time constant naming the source file being compiled; raku resolves it to
an absolute path regardless of how the file was invoked on the command line, while mutsu keeps
whatever path string was passed on the command line verbatim.

## Root cause guess

Wherever `$?FILE` is populated at parse/compile time, mutsu likely stores the raw CLI argument
path instead of canonicalizing it (e.g. via `std::fs::canonicalize` or similar) the way raku
does.

## Affected files (starting point)

- `src/main.rs` / `src/parser/` — wherever the source file path is captured and bound to
  `$?FILE`

## Priority note

Low-priority/cosmetic — this only affects diagnostic output, not program semantics — but simple
enough to fix opportunistically.

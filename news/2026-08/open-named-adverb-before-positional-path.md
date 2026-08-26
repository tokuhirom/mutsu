# `open :w, $path` — a named adverb written before the positional path

`open :w, "/tmp/f"` died with

```
Failed to open '.../w\tTrue': No such file or directory (os error 2)
```

— the `:w` Pair had been stringified (tab-separated) and used as the filename.
The equivalent `open "/tmp/f", :w` worked. Found by the doc-diff harness on
`Type/independent-routines.rakudoc:473`, whose own example uses the leading-adverb
spelling.

## Root cause

This is a runtime argument-binding bug, not a parse one: `--dump-ast` shows the
call already carries the Pair and the path as two separate arguments, in source
order. `builtin_open` (`src/runtime/builtins_io.rs`) then took `args.first()` as
the path and `&args[1..]` as the flags, assuming the path is always the
syntactically-first argument. Raku lets a named argument precede a positional one
at the call site, so `open :w, $path` puts the `:w` Pair in `args[0]` and the
path in `args[1]`.

## Fix

`builtin_open` now finds the path as the first *non-Pair* argument and treats
every Pair as a flag regardless of position, passing that Pair set to
`parse_io_flags_values`. The three places that previously indexed `args[0]` /
`args[1..]` — the `IO::Special` reopen sentinel, its `:r` probe, and the path
extraction itself — all use the partitioned values.

Verified against `raku` v2026.06 for `:r`, `:w`, `:a`, `:bin` and `:enc<utf8>`
before the path; for an `IO::Path` (not just a `Str`) as that path; for the
trailing-adverb spelling still working; and for adverbs on both sides
(`open :w, $path, :nl-out("\n")`). Pinned by `t/io-entry-points.t`.

# `lines`/`words` in sub form now read an `IO::Path`, like their method forms

The global `lines()` and `words()` routines took a different view of their
positional argument than the `.lines`/`.words` *methods* did. Handed an
`IO::Path`, both stringified it and split the resulting path text, so
`lines("f".IO)` returned a one-element `Seq` holding the path itself
(`("tmp/lines-test.txt",).Seq`) instead of the file's lines — while
`"f".IO.lines` read the file correctly. Found by the doc-diff harness on
`Type/IO/Handle.rakudoc:385` and the `lines` section of
`Type/independent-routines.rakudoc`.

## Root cause

`builtin_lines` and `builtin_words` (`src/runtime/builtins_io_stream.rs`) each
gate on `handle_id_from_value(first)`: an `IO::Handle` positional goes to the
handle-reading path, and *everything else* falls through to the string-splitting
path. That is right for a `Str` (`lines("a\nb")` really does split the string),
but an `IO::Path` is neither — the sub forms take an `IO()`-coercible positional
exactly the way `slurp` does, so an `IO::Path` has to be opened and read.
`slurp`/`spurt` already got this right; only `lines`/`words` disagreed.

Nothing about the reading logic was missing: `try_io_path_content_read`
(`src/runtime/native_io/io_path_read.rs`) is the single implementation the
`.lines`/`.words` method forms already use, covering `:chomp`, `:nl-in`, the
numeric limit argument and encoding.

## Fix

A new `try_io_path_content_sub` helper in `builtins_io_stream.rs` recognizes an
`IO::Path` (the built-in family, via `is_io_path_lexical_class`) as the first
non-Pair positional and delegates to `try_io_path_content_read` with the
remaining arguments. `builtin_lines` and `builtin_words` consult it first. The
two spellings therefore share one read+split implementation rather than growing
a second one, which is what let them drift apart in the first place.

Verified against `raku` v2026.06: `lines($path)`, `lines($path, 2)`, a repeated
call, `lines($handle)` (consuming), `lines("a\nb")` (string split), `.^name` of
`Seq`, `.is-lazy` of `False`, and the same matrix for `words`, all agree.

Pinned by `t/io-entry-points.t`, which also pins that `slurp`/`spurt` keep
agreeing with them.

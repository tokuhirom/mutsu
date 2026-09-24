# `$fh.comb` with no matcher combs the handle's characters

`"file".IO.open.comb` returned `().Seq`, while `.comb(/\w/)` on the same handle
worked. The handle's `comb` arm (`src/runtime/native_io/io_handle.rs`) slurps
the handle and delegates to `dispatch_comb_with_args`, which answered `None`
when there was no positional matcher, and the handle arm turned that `None`
into an empty Seq. `IO::Path.comb` had hit the same `None` earlier and patched
it with a private grapheme split of its own.

`dispatch_comb_with_args` now answers the no-matcher case itself through
`builtins::comb::comb_pure(target, None, ..)`, the same grapheme split
`Str.comb` uses, so the handle, the path and a named-only `Str.comb(:match)` all
share it. The private copy in `io_path_read.rs` is gone.

Pinned by `t/regex/handle-comb-no-matcher.t` (#9255).

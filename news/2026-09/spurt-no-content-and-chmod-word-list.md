# `spurt $path` with no content, and `chmod` on a word list of paths

`spurt "empty.txt"` died with "spurt requires a content argument" instead of
creating (or truncating) an empty file, the behavior Rakudo has had since
2020.12. The function-call form of `spurt` was the only entry point that
still required a content argument -- the `IO::Path.spurt` and
`IO::Handle.spurt` method forms already defaulted it to an empty string.

`chmod 0o755, <c1 c2>` stringified the whole word-list argument into one
bogus path (`'c1 c2'`) and died with "No such file or directory" instead of
flattening it into two separate paths, because `chmod`'s `*@filenames`
slurpy was never flattened the way `unlink`'s already is. `chmod` also threw
on the first path it could not chmod rather than silently dropping it from
the result, and returned a `List` (gisting as `(...)`) where Rakudo's
implementation returns an `Array` (`[...]`). All four now match Rakudo:
`chmod 0o755, <c1 c2>` gives `[c1 c2]`, and `chmod 0o755, <c1 nope>` gives
`[c1]`.

`unlink`, `mkdir`, and `rmdir` were checked for the same stringification and
were already correct.
(#9837)

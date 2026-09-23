# `CompUnit::Repository::FileSystem.load(IO::Path)`

`CompUnit::Repository::FileSystem` gained its `.load(IO::Path)` method, used by
zef's `Zef::Client` on its `CompUnit::Repository::Staging` fallback
(`$*REPO.load($curs-handle.path)`) and previously missing entirely
(`No such method 'load' for invocant of type
'CompUnit::Repository::FileSystem'`).

`.load` resolves the given file relative to the repository's own prefix and
compiles it as a compunit — a syntax error in the file surfaces here, sharing
the same `parse_module_source` path `need` uses — and records the resulting
`CompUnit` in `.loaded`. Verified against `raku`: unlike `need`, `.load` does
**not** merge the file's symbols into `GLOBAL` (a `unit module`'s subs stay
unreachable via a fully-qualified call afterward, and `.handle` is always an
empty `CompUnit::Handle`), and its `short-name` is the bare relative file
name rather than a `::`-joined module name. `raku` also rejects an absolute
path outright (it searches the registered `$*REPO` chain, which a repository
built with `.new` was never added to); mutsu instead accepts an absolute path
when it canonicalizes to somewhere under the repository's own prefix, which
covers zef's own use (a path it already resolved from this same prefix)
without reproducing that unrelated chain lookup.

See [issue #9079](https://github.com/tokuhirom/mutsu/issues/9079).

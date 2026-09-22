# `slurp`/`spurt` free functions now honor an IO::Path's captured cwd

Found while working the `cro` distribution (locked on the ecosystem lock board,
[#8977](https://github.com/tokuhirom/mutsu/issues/8977)): `Cro::Tools::Link::Editor`'s own test
suite (`t/tools-link-editor.rakutest`) builds an `IO::Path` before `chdir`-ing into a fixture
directory, then reads and writes that path again with the free-function forms `slurp($path)` /
`spurt($path, ...)`.

An `IO::Path` built via `.IO` (or `IO::Path.new`) captures `$*CWD` into its own `cwd` attribute at
construction time, and the `.slurp`/`.spurt` *methods* on that instance already resolved a relative
`path` attribute against that captured `cwd` (`resolve_io_path_buf`). The free-function forms did
not: `builtin_slurp` stringified the argument and handed the (possibly relative) string straight to
a raw `fs::read`/`fs::read_to_string`, and `builtin_spurt` resolved it against the *live* virtual
`$*CWD` via `self.resolve_path` — in both cases discarding the `IO::Path`'s own captured directory.
A `chdir` between constructing the path and reading/writing it (a common pattern: capture a fixture
path, `chdir` into a sandbox, operate on the path again) silently re-resolved against the wrong
directory and mutsu died with a bogus "No such file or directory", while Rakudo passed.

Fixed with a shared `resolve_io_arg_path` helper that resolves an `IO::Path` argument through the
same `resolve_io_path_buf` the methods use, falling back to the existing `$*CWD`-relative resolution
for a plain string argument. Wired into `builtin_slurp` and `builtin_spurt`.

Pinned by `t/io/slurp-spurt-io-path-captured-cwd.t`. `cro` 0.8.10 moves from `partial` (1/2 baseline
files, 75/82 assertions) to `green` (2/2 files, 82/82 assertions).

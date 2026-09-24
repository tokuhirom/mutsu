# `CompUnit::Loader.load-source-file` runs helper scripts

`Test::Script` uses `CompUnit::Loader.load-source-file($path.IO)` to execute
helper scripts while redirecting `$*OUT`/`$*ERR`, then reads their global
variables through the returned handle. mutsu had only the source-buffer form,
so the distribution's six baseline files either saw no output or died with a
missing-method error.

mutsu now reads the `IO::Path`, runs the source in the caller's dynamic scope,
dispatches its `MAIN` with inherited arguments, and returns the existing
`CompUnit::Handle` shape with its globalish package available. The loader also
keeps a nested package's local captures separate from a parent package's
private lexicals.

Pinned by `t/modules/compunit/compunit-loader-source-file.t`, reduced from
`Test::Script` 0.0.4.

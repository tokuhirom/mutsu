# `CompUnit::Repository::Installation` / `::FileSystem` can be subclassed; Rakudo's `Staging` loads verbatim

A user class deriving from a repository class (`class CompUnit::Repository::Staging is
CompUnit::Repository::Installation`) failed "cannot inherit ... because it is unknown", and even when
declared its instances answered none of the native rows. Both classes are inheritable now; a
subclass instance carries `prefix` / `next-repo` / `short-id` itself plus a reserved marker the
method table reads (`carried_state`), so the repository rows answer on the instance with its own
methods taking priority. Rakudo's own 82-line `CompUnit::Repository::Staging` is vendored verbatim
into the Rakudo-Core battery and runs: `deploy`, `self-destruct`, `short-id`, `path-spec`.

Around it: `Installation.install` honours `:force` (replaces the identical installed distribution),
`Installation.name` / `CompUnit::RepositoryRegistry.name-for-repository` answer the registered or
built-in name. Pakku's `add` now stages, installs and runs the stage's tests (mutsu#14914);
the tests' child process then needs `RAKULIB` (mutsu#14918).

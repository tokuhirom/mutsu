# The primitives Pakku's `add` path needs: `register-name`, `next-repo =`, `DIR-RECURSE`, `Distribution::Locally`

Past `CompUnit::Repository::Spec` (the previous entry), `Pakku::Core.repo-from-spec` and
`Pakku::Meta.to-dist` hit four more gaps, each now closed against `raku`:

- `CompUnit::RepositoryRegistry.register-name($name, $repo)` registers a repository that
  `repository-for-name($name)` answers afterwards.
- `$repo.next-repo = $other` assigns a repository's link in place (the attributes are shared, as
  Rakudo hands out one object per prefix).
- `Rakudo::Internals.DIR-RECURSE($path, :$dir, :$file)` lists the files below a directory.
- `Distribution::Locally` is a builtin role (`prefix`, and a `content` that returns an unopened
  `IO::Handle`). An unopened `IO::Handle.opened` is `False` now (it died "Expected IO::Handle"), and
  a relative `IO::Path` opens against its own `CWD`.

Pakku's `add` then stops at `CompUnit::Repository::Staging` (mutsu#14914).

# `CompUnit::Repository::Spec`

The class that `CompUnit::Repository::Spec.from-string($text, $default-short-id)` answers did not
exist, and `CompUnit::RepositoryRegistry.repository-for-spec` took only a string. It is a builtin
Raku-source prelude now (attributes `short-id`, `Str`, `path`, `options`, split rules measured
against `raku`), and `repository-for-spec` accepts it. Pakku's `repo-from-spec`, which every `add`
with a `to` target goes through, gets past it (mutsu#14906).

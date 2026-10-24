# Repository `.candidates` honours a `CompUnit::DependencySpecification` subclass

`CompUnit::Repository::{Installation,FileSystem}.candidates` only understood the plain native
specification: a subclass instance (Pakku::Spec's `Pakku::Spec::Raku`, which overrides
`version-matcher` / `auth-matcher` with arbitrary selectors) matched nothing, even by name. The
short-name is read from the subclass's native payload now, and the subclass's own matchers are
called and smartmatched against the distribution's auth, version and api, as Rakudo does. Pakku's
`spec` suite passes completely (mutsu#14870).

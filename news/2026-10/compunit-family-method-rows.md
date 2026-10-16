# The CompUnit family's methods are method-table rows

The repository API (`CompUnit::Repository::FileSystem`, `CompUnit::Repository::Installation`), `CompUnit`,
`CompUnit::Handle` and the distributions (`Distribution::Hash`, `Distribution::Path`, a module's `$?DISTRIBUTION`, an
installed distribution) no longer answer through name-matching arms in the slow path: each method is a row of the
method table, owned by the class Rakudo declares it on and found by that owner (ADR-11276 §9.117).

On the way the family moved closer to Rakudo: `$?DISTRIBUTION` is a `CompUnit::Repository::Distribution` whose `.Str`
is the distribution's identity (`Foo:ver<0.1>:auth<zef:x>:api<>`), an installed distribution is a
`CompUnit::Repository::Installation::LazyDistribution`, a FileSystem repository's `.prefix` is an `IO::Path`, and a
FileSystem repository has no `install` method.

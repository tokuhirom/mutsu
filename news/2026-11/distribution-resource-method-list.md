# A `%?RESOURCES` entry answers rakudo's short list of path methods

`Distribution::Resource` shared every `IO::Path` row, so `$resource.e`, `.parent`, `.child`, `.add`,
`.chmod`, `.watch`, `.modified`, `.path` and the rest answered, and `$resource.rename(...)` /
`.move(...)` really moved the distribution's file, where rakudo says "No such method". A resource
now answers only the `IO::Path` methods rakudo's class has (`IO`, `absolute`, `basename`, `comb`,
`copy`, `dirname`, `extension`, `gist`, `is-absolute`, `is-relative`, `lines`, `open`, `parts`,
`raku`, `relative`, `resolve`, `slurp`, `split`, `volume`, `words`, `Str`, `ACCEPTS`, `Numeric`,
plus `platform-library-name`); the list lives in one place
(`builtins/method_table/distribution_resource.rs`) and gates both the method table and the native
`IO::Path` delegation. `$resource.parent` no longer trips the method-table lane assertion in a debug
build. Closes #16206.

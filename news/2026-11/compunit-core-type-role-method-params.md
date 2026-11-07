# Role methods may name the remaining `CompUnit::` core types

A role method signature naming `CompUnit::PrecompilationRepository` (the
`CompUnit::Repository` role of Language/compilation.rakudoc) was rejected with
"Invalid typename ... in parameter declaration" when the role was declared,
although the identical signature on a sub or class method was fine. The
compound core type list behind that check now also knows `CompUnit::Handle`,
`Loader`, `PrecompilationRepository` (and `::None`), `PrecompilationUnit` (and
`::File`), `Repository::AbsolutePath` and `Repository::Unknown`, all of which
rakudo resolves. Found by the raku-doc differential sweep.

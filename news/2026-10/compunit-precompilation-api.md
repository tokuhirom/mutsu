# CompUnit precompilation API: stores, ids and `try-load`

`CompUnit::PrecompilationStore::File` / `::FileSystem`, `CompUnit::PrecompilationId`,
`CompUnit::PrecompilationDependency::File` and `CompUnit::PrecompilationRepository::Default`
now exist as Raku-level prelude classes (`src/runtime/run_prelude_precomp.rs`), injected into any
compunit that names `CompUnit::Precompilation*`. mutsu still loads from source, so `try-load`
compiles the dependency as its own compunit and returns a `CompUnit::Handle` whose `.unit`
exposes `$=pod` and `$?PACKAGE`; nothing is written under the store's prefix. This is what
Pod::Load needs to read the Pod of a file (#11541). `CompUnit::Handle.new` is a native
constructor over a unit hash.

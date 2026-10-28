# `is native` in a later importer applies through its own unit's import

NativeCall's `is native` is a `trait_mod:<is>` candidate supplied by its `sub EXPORT`. A routine that
declares a nested `my sub ... is native {*}` applies the trait each time it runs, and it looked the
`&trait_mod:<is>` dispatcher up in the running env. When a script had loaded an earlier importer
whose plain `is export` import of NativeCall left the hook-less dispatcher in that env (any unit
declared `:ver<...>`/`:auth<...>`), the trait found no candidate and was dropped silently, so the
native `memcpy` answered its `{*}` body (`Whatever`). The dispatcher now comes from the executing
unit's own recorded import first. `NativeHelpers::Blob`'s `blob-from-pointer` under
`Archive::Libarchive` works; all seven Archive::Libarchive test files match `raku`.

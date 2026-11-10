# A slang module's nested `use` of another slang is no longer handed to the importer

`$*LANG.define_slang` slangs the compilation unit being compiled. A slang module such as
Qwiratry's `Qwiratry::Mold::Slang` that merely `use`s another slang (the Topic slang, which
rewrites `given` into a call to a helper the importer never imported) used to pass that slang on
to every importer: its `given` became `qwiratry-given-topic(...)`, which died with
`Unknown function`. Rakudo leaves the importer's `given` alone, so the registration made by a
module nested inside the activating module's own load is now ignored for the importer
(#17267). `t/transformer.rakutest` now runs past its third subtest.

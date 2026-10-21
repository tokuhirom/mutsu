# `require "M.rakumod"` declares a package-less file's types under their own names

The path form of `require` guessed a package from the file name and ran the
file's top level inside it, so `require ("GH2983.rakumod") <C-GH2983>` named its
type `GH2983::C-GH2983`, where Rakudo (and the `require GH2983` form) says
`C-GH2983`. The file's top level now always runs in GLOBAL; the guessed package
only adds the `pkg::name` function and env aliases it always did (#14336).

Rakudo runs the file again for the path form (the two types are not `===`), so
the issue's "reuse the loaded compunit" direction was not adopted.

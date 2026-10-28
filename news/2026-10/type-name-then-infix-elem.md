# A type name followed by `(elem)` is an infix, not a call

`ok IO::Path (elem) $navigator.supported-types, '...'` died with "Two terms in a row":
the listop-head rule read any identifier followed by `(` as `name((elem) ...)`. A
built-in type name (`Int`, `Str`) or a package-qualified name ending in a capitalised
component (`IO::Path`) is a complete term, so what follows is an infix. Qwiratry's
`t/integration/filesystem-tree-navigator.rakutest` now compiles.

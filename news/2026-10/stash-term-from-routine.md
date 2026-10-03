# A package term stored through the stash from a routine stays readable

`Logic::Ternary` builds its `True`/`Unknown`/`False` values inside `sub
EXPORT` with `Logic::Ternary::{'True'} = Logic::Ternary.new: 'True'`. The stash
assignment already kept the value in the durable package store. A later read of
`Logic::Ternary::True` never consulted that store, though: once the routine's
env was gone, the qualified bareword fell through to an auto-vivified package
named `Logic::Ternary::True`. The values were therefore type objects without a
`key`, so `//` (which calls the class's own `.defined`), smartmatch and string
contexts all went wrong.

A qualified bareword that names no type now reads the package store before it
is treated as a package name. All five of Logic::Ternary's test files pass.

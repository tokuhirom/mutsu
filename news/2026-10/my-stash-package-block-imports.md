# `MY::` inside a package block lists that block's imports

`module M { use Foo; MY::.keys }` now lists the routines `use Foo` imported,
even before any of them has been called. A package block opens no run-time
import scope of its own, so the lexical-stash read for it found no imports at
all; it now reads the import aliases recorded for the package.

P5built-ins builds its whole export list this way
(`%export = MY::.keys.grep(*.starts-with('&'))...` right after its `use`
statements), so it exported nothing but `&abs` and a few others; its test now
passes 97/97.

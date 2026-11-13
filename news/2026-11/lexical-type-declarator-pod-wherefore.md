# A `my class` declarator block documents the lexical type

A lexical type (`my class`, `my module`, `my package`, `my subset`) is
registered under its declaration's storage name, but its `#|` block's
`$=pod` entry pointed its `WHEREFORE` at a package of the bare spelling: a
`ClassHOW` stand-in that was not `===` to the type, and the type's own `.WHY`
found no documentation. Pod::To::Man, which renders a declarator block by
asking its `WHEREFORE` for `.HOW` and `.WHY`, documented a `my module` as a
class and lost the doc text.

The declarants built for `$=pod` now carry the lexical type's storage name
(computed from the declaration's site id the same way registration does), and
`.WHY` on a lexical type also looks under its declared name. Pod::To::Man's
test suite now matches raku.

This also retires the `.HOW` fallback that guessed a lexical package's
storage name from the bare spelling by scanning every declared package kind
(and gave up when two lexical packages shared a name): the `WHEREFORE` now is
the package itself.

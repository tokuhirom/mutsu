# RakuAST retains written routine traits

Routine declarations now retain their written trait order and arguments for
RakuAST, including combinations of `is rw`, `is raw`, `is export`, custom
traits, return traits and operator precedence. This covers named subs,
methods, protos and anonymous routines through the same typed source record.
Ordinary parses do not allocate the record.

Explicit `is export(:DEFAULT)`, parenthesized associativity and angle-word
custom arguments keep their source forms. `returns Positional of Int` is
represented as one parameterized return trait.

Proto custom-trait arguments are compiled using the existing declaration
expression chunks. Traits receive the live dispatcher rather than a detached
copy of its `{*}` body, and proto return traits reach its signature.

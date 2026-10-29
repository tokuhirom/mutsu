# `$*CU` is the unit's RakuAST::CompUnit at compile time

`BEGIN say $*CU.^name` and what a `use`d module's `EXPORT` sub reads while the importing
unit compiles now see a `RakuAST::CompUnit` (built once per unit from its source text,
`src/unit_source_text.rs`); at run time `$*CU` stays a Failure, as in rakudo. `static::CLI`
no longer dies with "Dynamic variable $*CU not found" (#14972).

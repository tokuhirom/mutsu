# RakuAST keeps `is export` on constants and variables

`constant k is export = 8` and `our $v is export(:t) = 3` now round-trip
through RakuAST: the converter renders the export as rakudo's
`Trait::Is(export)` instead of silently dropping it, and lowering turns the
trait back into the declaration's export flag. Under `MUTSU_RAKUAST=1` an
exported constant nested in a routine body is exported again.

The importer's module scan also learned three ways a module exports a type
name that it used to miss -- a nested `my class X is export`, a
`class GLOBAL::X`, and the short name of an exported qualified class -- so
those bare type names resolve at parse time and convert as `Type::Simple`.
Nine `t/modules/` files move into the RakuAST frontend ratchet;
`t/rakuast/rakuast-exported-declarations.t` pins the node shape and the
round trip.

# A routine's return type is the one its declaring scope names

`sub f(--> Lex)` with `my class Lex`, or `--> Alias` with
`my constant Alias = Lex`, now answers `Lex` from `.returns` and
`.signature.returns` even when another module asks. Before, the spelling was
resolved where the question was asked, so a module that received `&f` got
back an unrelated, empty type object of the same name. The routine records
the type object it found in its declaring scope on its identity cell
(ADR-11827).

A callable parameter's signature also keeps its return type now: for
`&cmp (Pointer, Pointer --> int32)`, `.sub_signature.returns` is `int32`.
Upstream NativeCall reads it to marshal a C callback's result. With both
fixes, `qsort` callbacks and `constant`-aliased CStruct return types work
through the vendored module. That brings 59 of the 66 `is native` test
files to passing with the switch applied.

# Type names are qualified as each top-level statement is finalized

A bare type name in a type position inside a package (`class C { role R {};
method m(R $x) {} }`) is written as its qualified name, `C::R`, so the run time
resolves the right type. That qualification used to be a second walk over every
finished compilation unit (`parser::qualify_type_names`), after the
finalization had already walked it, and it collected the `our` types the unit
declared anywhere before qualifying the first statement.

The finalization now does it itself: as it reaches each top-level statement it
qualifies that statement's type positions from the packages declared so far
(`parser::UnitQualify`; an open `unit` package carries over to the statements
after it). The separate walk is gone from the executing path; it stays only for
`EVAL($ast)`, whose hand-built tree is never finalized. This is slice 2c of
ADR-15760 Amendment 2 (#15760).

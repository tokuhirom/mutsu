# A BEGIN-time error no longer runs the unit's END phasers

`BEGIN die "x"; END note "e"` printed `e` before the BEGIN error, because the
ADR-0134 BEGIN prologue is compiled into the mainline and its error came back
like any run-time one, which then ran the END queue. In rakudo the unit never
finished compiling, so no END runs.

The main unit's prologue (and the undeclared-routine guards that follow it)
now ends with a `Stmt::BeginPrologueEnd` marker, compiled to the
`EndBeginPrologue` opcode, which lowers an interpreter flag. An error that
escapes the mainline while the flag is still raised is a compile-time failure:
`run` returns it without running the END phasers, the same as the statically
detected undeclared-routine error. This covers a dying `BEGIN`, an undefined
`use Foo:if(...)` condition and the `use :if(False)` undeclared-routine guard
(#10977).

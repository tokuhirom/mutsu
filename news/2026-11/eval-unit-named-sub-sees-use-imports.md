# A named sub of an EVAL'd unit sees what the unit's `use` imported

`EVAL 'use NativeCall; sub f() { Pointer }; f()'` answered `Any`, and
`Pointer[int8]` in it died with `Any cannot be parameterized`: `Pointer` is a constant
`NativeCall` exports, and a routine written at a unit's top level hides the names that
an open block-scoped `use` imported (ADR-8751). The `use` of an `EVAL` unit opens such a
scope too, and no routine of the program is written in it, but a routine the unit itself
declares is. `scope_encloses_code` now counts the `EVAL` unit's own scope as enclosing
the routines compiled from that unit. A `use` inside a block of the unit stays hidden
from a sub outside the block.

The `EVAL(Q{...}.AST)` entry still hides the names: its routines carry the file of the
program that EVALs (tracked on #16207).

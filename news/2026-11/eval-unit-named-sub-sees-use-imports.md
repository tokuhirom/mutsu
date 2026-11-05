# A named sub of an EVAL'd unit sees what the unit's `use` imported

`EVAL 'use NativeCall; sub f() { Pointer }; f()'` answered `Any`, and
`Pointer[int8]` in it died with `Any cannot be parameterized`: `Pointer` is a constant
`NativeCall` exports, and a routine written at a unit's top level hides the names that
an open block-scoped `use` imported (ADR-8751). The `use` of an `EVAL` unit opens such a
scope too, and no routine of the program is written in it, but a routine the unit itself
declares is. `scope_encloses_code` now counts the `EVAL` unit's own scope as enclosing
the routines compiled from that unit. A `use` inside a block of the unit stays hidden
from a sub outside the block.

`EVAL(Q{...}.AST)` goes the same way: the lowered unit's routines carry the file of the
program that EVALs it, so the unit's import scope now has a region key of its own
(`ModuleState::eval_unit_region`) that the compile of the unit's statements opens
(`Compiler::seed_import_region`), and `scope_encloses_code` finds it among the routine's
`enclosing_import_regions`. Six more `t/` files run as `EVAL(slurp($f).AST)`.

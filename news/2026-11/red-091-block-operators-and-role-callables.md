# Red t/091 matches raku: block-local operators, `++` through an imported family, `does R[&a]`

Red's RakuAST compiler test now gives the same 8/8 as rakudo. Making it pass
took fixes in several places, each covered by its own test.

**Block-local operator candidates.** A block's `multi infix:<+>(Red::AST, Int)`
now wins over the candidates `use Red` imported, wherever the block's code
runs:

- A map/grep callback is compiled again as a loop body. That recompile now
  keeps the routines declared around the closure shadowing the imports.
- A family that a `sub EXPORT` hook handed the unit now also contains the
  unit's own candidates of the operator.
- A bare block in a closure's tail position that declares a routine is a
  routine scope of its own. Before, the candidate outlived the block, and a
  sibling block's identical candidate was then ambiguous.

**The CHECK-time value of a block's `multi`.** It is the candidate the
declaration compiles to, matched by file and line. It used to be whatever sat
at the declaration's index in the dispatcher, which for an operator was one of
the setting's candidates. If the block has not run yet, the candidate is
compiled from the declaration. That compile skips the EVAL undeclared-name
check: the declaration already passed the unit's own check, and Red's walker
runs in Red's module, which does not see `ast-value` or the other names the
test imported. A routine that has code of its own lists only itself in
`.candidates`, even when it is named like a built-in operator.

**`++` through an imported family.** Inside a tree run as an EVAL unit (Red's
CHECK-rewritten unit), a routine called back from another module ran `++$x`
through the family the frame bound as `&prefix:<++>`. The family received the
value instead of the container, so the increment was lost. `++`/`--` now rank
the core candidates first, through the operand's container.

**An unsupplied `&cb?` role parameter after `does`.** It is the `Callable` type
object also when the role is mixed in place: `$attr does R[&ref]`, as reblessed
since #17766. It used to read as Nil in the role body. Red's
`Red::Attr::Relationship.transfer` re-parameterises its own role with
`&rel2`, so every Red test that defines a relationship died with "No matching
candidate found for the parametric role" (t/001, t/004, t/010 and about 25
more).

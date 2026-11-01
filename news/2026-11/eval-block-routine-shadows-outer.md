# A routine declared in a block of an EVAL unit shadows the outer one

Inside an `EVAL`, a `sub` (or operator) declared in a block nested in the unit, with the same name as
one the unit declares outside it, was refused as `Redeclaration of routine`, although the same text run
outside an `EVAL` shadows it and gets the outer one back after the block. The EVAL path switched the
lexical-shadow exemption off for the whole unit, so a name the unit's own outer scope declared collided
with the nested one. The exemption is now off only at the unit's own top level: the registration records the
block depth at which the running `EVAL` unit's statements run (`ModuleState::eval_unit_block_depth`)
and a block deeper than that, or an inlined block frame, follows the ordinary rules. A redeclaration at
the unit top level is still `X::Redeclaration`.

Found by the `EVAL(slurp($f).AST)` ratchet (#15759). It is not specific to `EVAL($ast)`: a string `EVAL`
had the same problem. Of the 119 `t/` files that failed through the ratchet with a `planned N, ran fewer`
or a generic block error, 15 now pass; 27 still fail with a `Redeclaration` of another shape (a
parametric role group `role R {...}; role R[::T] {...}`, forward declarations, a `BEGIN` prologue).

# An EVAL unit's own `use` is visible to its lvalue calls

A routine the top level (or a block) of an `EVAL`'d unit imports with `use` was
missing from the import chain the lvalue-call path reads: `EVAL q[use P5substr;
substr($s, 4) = 'dy']` died with `Unknown call: substr`. The chain now starts with
the sets the EVAL's own import scopes filled, then the sets around the `EVAL`
call. Three more `t/` files join the `EVAL(slurp($file).AST)` ratchet (#16356, #15868).

A routine the `EVAL` unit declares itself still wins over what its own top-level
`use`s import (`use M <shadowed>; sub shadowed {...}; &shadowed()`).

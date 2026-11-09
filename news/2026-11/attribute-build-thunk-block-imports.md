# An `Attribute.build` thunk sees its unit's block-scoped imports

The `.build` thunk of an attribute default ran in the env of whoever called it, so a module's
top-level routine (`run-attr-build`) hid the declaring EVAL unit's `use` from it:
`has Str $.id = ~BuildThunkDep.new` died with `Undeclared name` when `.build` was called from
another module under `EVAL(slurp($file).AST)` (or any string `EVAL`). The thunk now runs in a
frame of its own. `t/oo/attribute/attribute-build-thunk-unit-scope.t` joins the EVAL ratchet.

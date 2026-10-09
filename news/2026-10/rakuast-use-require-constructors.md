# RakuAST::Statement::Use and ::Require are constructible

`RakuAST::Statement::Use.new(module-name => ...)` used to die with
`Could not find symbol '&Use'` because the class had a model but no
constructor. `Use` (optional `argument`) and a new model-only `Require`
(optional `file` and `argument`) now build from the Rakudo constructor shape
and match rakudo's `.^name`, accessors and `Statement` ancestry. Lowering of
`Require` is still tracked in #7564.

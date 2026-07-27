# Construct RakuAST where-constrained parameters

`RakuAST::Parameter.new` now accepts a `where` constraint node. Constructed parameters retain the
node for `.where`, advertise the accessor through model introspection, and render the same
constructor shape as Rakudo.

When a constructed signature is evaluated, the model node lowers to the existing internal
`ParamDef.where_constraint` representation and continues through the normal compiler and VM.
The dual-oracle regression test passes under both Rakudo and mutsu.

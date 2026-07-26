# Construct RakuAST blocks

RakuAST construction now includes `RakuAST::Blockoid.new(StatementList)` and
`RakuAST::Block.new(body => Blockoid)`. Constructed nodes match Rakudo's rendering, expose their
children through ordinary model accessors, and report the new API through local method and
attribute introspection.

The dual-oracle regression test runs under both Rakudo and mutsu and composes the constructors with
the existing mutable `StatementList` and statement/literal constructors.

# RakuAST: the `RakuAST::Doc::Block` class

`RakuAST::Doc::Block.new(type => ..., level, directive/abbreviated/for, config, paragraphs, margin)`
now builds a node that prints (`.raku`) as rakudo's does, answers `type`, `level` (a string, `""`
for none), `delimited`, the flags, `paragraphs` and `config` (a `Map`), and keeps rakudo's flag
precedence (`directive` beats `abbreviated` beats `for`). The accessor method rows were regenerated
with `scripts/gen-rakuast-accessor-rows.py`. Reading a Pod block out of source as a statement of
`.AST`, `Doc::Markup`, `Doc::Declarator*`, `Var::Doc` and lowering to `$=pod` are the next slices
of #14880.

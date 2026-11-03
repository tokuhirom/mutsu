# RakuAST: the configuration of a Pod block

A Pod block written with configuration (`=begin foo :kind("Type") :category<basic> :flag :!off
:n(3)`, `=for foo :a(1+2)`) is now a `Doc::Block` statement with its `config` pairs, as rakudo's:
`:name(...)` and `:name<...>` hold the value node of the colonpair (`Circumfix::Parentheses`, a
words `QuotedString`), a bare `:name` holds `True` and `:!name` `False`. Each value is read as a
fragment of the unit's text, positioned in it (`pod_blocks.rs`). Adjacent pairs (`:a<x>:b<y>`), which
rakudo rejects, and anything else that is not such a list leave the block out as before. Not
emulated: rakudo lists a block with configuration that directly follows another block before it.
Part of #14880.

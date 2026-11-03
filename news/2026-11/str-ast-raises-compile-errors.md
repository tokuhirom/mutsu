# `Str.AST` raises the compile errors running the code raises

Rakudo's `Q{say A; class A {}}.AST` is `Illegally post-declared type` and
`Q{say x; my \x = 1}.AST` is `Undeclared routine: x`; mutsu returned a tree for
both. `Str.AST` now runs the post-declared-type check and the term-order check
(ADR-15760 Amendment 1, slice 1c) on the parsed unit, before converting it.
A sigilless declaration in the typed parse tree now reports its name to the AST
visitor, which the term-order check needs.

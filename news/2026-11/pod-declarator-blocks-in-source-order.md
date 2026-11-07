# `$=pod` lists declarator blocks in source order

`#|` declarator blocks were appended to `$=pod` after every other Pod block, so a script that wrote
`#| doc` / `sub` / `=begin pod` ... had `$=pod[0]` the Pod block where rakudo has the declarator.
The Pod scan now records where each block starts and the declarator blocks are merged by the
offset of the declaration they document. A tree evaluated with `EVAL($ast)` has no offsets and
keeps the declarators last. Closes #17070.

# RakuAST blocks capture enclosing underscore lexicals

RakuAST distinguishes `@_` and `%_` supplied by an enclosing routine from a
block's own slurpy placeholder declarations. Blocks inside routines, methods,
method literals, and pointy blocks retain their enclosing lexical arguments
and their ordinary topic parameter. An unbound first use still declares a
placeholder; subsequent references and nested blocks use that binding.
Constructed RakuAST placeholder nodes continue to bind fresh positional and
named arguments through the compiler's existing closure path.

Explicit `-> *@_` and `-> *%_` blocks keep their pointy declarator throughout
parsing and lowering, so their written signatures are preserved and their
parameters shadow enclosing bindings.

A single implicit-block classifier also preserves combinations of scalar,
array and hash placeholders, including their use as regex arguments.

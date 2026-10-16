# A top-level sub's EVAL no longer sees its calling block's lexicals

A sub declared at the top of a program that looks a variable up by string
(`EVAL '$z'`, `::('$z')`) used to find a `my $z` declared only in the block
that called it, and a variable a block-scoped `use` imported. rakudo reports
neither: they are not in the sub's lexical outer. mutsu now agrees.

The compiler records which lexicals the program's nested blocks declare and
hands the ones the top level does not declare to each such sub. When the sub
is called, its frame hides those names, plus the variables of any open
block-scoped import, from its view of the program scope (#13604, ADR-12529
phase 3).

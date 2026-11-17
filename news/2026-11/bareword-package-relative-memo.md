# A module's own class names are resolved once per site, not on every read

A bareword naming a type of the running module's own package -- `Cell` in
`unit module Markdown::Lex`, which means `Markdown::Lex::Cell` -- went through
the whole term-resolution chain on every execution. The per-site bareword memo
(ADR-0121 D3) only remembered an answer spelled exactly like the name (`P` for
`P`), so every `Cell.new` in a module routine paid the running-package
`our`-store walk (seven `get_our_var` probes, most of them misses) plus the
module-table probes: about 41,000 instructions per read.

Such an answer is now remembered too, together with the running-package
context it was resolved under (the running method's class, the scope's and the
frame's lexical package, the frame's package and the current package). A hit
is used only under an equal context, so a routine reached from a method of a
class that declares its own `Cell` still resolves the name afresh; the other
re-checks of the memo (env bindings of the name and its term key, visibility)
are unchanged.

Measured with callgrind on a release build: `my $c = Plain` in a loop inside a
module routine went from 41.5k to about 1k instructions per iteration, and a
row of a Markdown::Lex pipe table (`| 1 | 2 |`, two `Cell.new` plus three
`Span` reads) from 1.62M to 1.36M (-16%). Refs #16874.

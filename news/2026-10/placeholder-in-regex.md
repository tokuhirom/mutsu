# Placeholders in regex literals

`my &f = { so "abc" ~~ /$^a/ }` made a 0-arity block: the match regex reaches
the AST as a regex value that keeps only its source text, and the placeholder
collectors (the typed-visitor walkers of ADR-0137) had no node to see. They now
scan a regex literal's source (`src/ast/regex_placeholders.rs`), so a
placeholder interpolated into `/…/`, `rx/…/` or `m/…/` is the enclosing
block's parameter — `&f.arity` is 1 and `f("b")` is `True`, as in Rakudo.

The same scan tells a placeholder inside a regex code block (`/<?{ $^a }>/`,
`/a { $^q }/`) apart: such a block takes no signature, and it now raises
Rakudo's `X::Placeholder::Block` before the program (or the `EVAL`'d snippet)
runs, instead of being silently accepted (mutsu#10542).

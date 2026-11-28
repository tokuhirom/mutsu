# A sigilless term may be named like a statement prefix

`my \lazy = (1, 2, 3); say lazy[1]` printed `(...)` and `lazy.elems` died:
mutsu always read `lazy`, `eager`, `hyper`, `race`, `sink` and `quietly` as
statement prefixes, even when a sigilless term or constant of that name was in
scope. `my \sink` and `constant quietly` were not even registered as terms.

mutsu now follows rakudo's rule. A declared term spelled like a statement
prefix is the term where no whitespace follows the word (`lazy[1]`,
`lazy.elems`, `race*2`, `sink+1`, `quietly;`). Where whitespace follows, it is
still the prefix (`sink 3`, `lazy + 1`, which is `lazy(+1)` in rakudo too).
One predicate, `declared_term_shadows_prefix`, decides it for the prefix
branches, and the shared term matcher applies the same rule (#19364).

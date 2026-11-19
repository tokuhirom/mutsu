# A hash composer with a spread element no longer calls `&hash` by name

A `{ ... }` hash composer holding a non-pair element (`{ b => 2, %x }`,
`{ d => 4, |@l }`, `{ e => 1, $c ?? (:f) !! () }`) compiled to a by-name call
of the routine `hash`, so a program's own `sub hash` took the composer over
(`(Str) (user-hash)` where Rakudo prints `(Hash) (a => 1 b => 2)`), and every
evaluation paid the general function dispatch (~38k Ir against ~8.5k for an
all-pair composer).

The parser now keeps such a composer as its own node, a `HashComposer`
contextualizer over the list of its elements, which the compiler lowers to
`.flat.hash` (the composer flattens its elements, as rakudo's does), and the
RakuAST boundary shows and lowers as `Circumfix::HashComposer` over the comma
list, so `.DEPARSE` prints `{b => 2, %y}` (#18079).

# Built-in methods fail an arity mismatch the way rakudo's dispatcher does

A built-in method called with a positional count none of its candidates takes now raises the error
rakudo raises. `"abc".lines("x", "y")`, `(1, 2).pick("a", "b")`, `"abc".comb(1, 2, 3)` and `5.base`
are `X::Multi::NoMatch` with rakudo's candidate list (they said "Too many positionals passed", "No
matching candidates for method: comb" and "No such method 'base'"), while `"abc".chars(1)` and
`"a".uc(1)` -- whose protos are `proto method chars(*%)` -- and the only method `List.eager` keep
"Too many positionals passed" (`List.eager(1)` was a NoMatch).

The generated candidate table (`scripts/builtin-method-sigs.raku`) now records every multi, a
one-candidate multi too (`Any.Array`), a `-` row for an only method (which hides an ancestor's multi
of the same name), and a `#<arity>-<count>` row for a proto that binds a bounded number of
positionals. A list invocant is named `List` in the message, not `Array`. Remainders (1) and (2) of
#16736.

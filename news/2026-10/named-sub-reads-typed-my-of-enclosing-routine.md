# A named sub reads a constrained scalar of its enclosing routine

```raku
sub a() { my Bool $c = True; sub f() { $c }; f() }   # was Nil, now Bool::True
```

A named sub declared inside a routine read a `my Bool`/`Any`/`Cool`/`Array`
scalar of that routine as `Nil` on every call (`my Int`/`Str`/`Num`/`Rat`
and untyped variables were fine). The hoisted registration of the sub seeds an
alias cell for the variable before its declaration has run; for a variable that
cannot be boxed (`type_constrained_unboxable`) the in-sequence registration
declined to make a cell and left the seeded `Nil` alias behind, so the sub read
it instead of the variable. It now drops the stale alias and the sub resolves
the variable by name, so later assignments from either side are seen too.
`LLM::RetrievalAugmentedGeneration`'s `VectorDatabase.join` (`my Bool:D
$commonKeys` read by `sub joined-key`) lost vectors on every join after the
first; `t/02-vector-database-join.rakutest` now matches `raku` (12/12).

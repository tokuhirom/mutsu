# A hyper operator over a `Positional`-but-not-`Iterable` object

`$carray <<*>> $carray` answered `0` in mutsu; rakudo dies `X::Multi::Ambiguous`
(a `CArray` is `Positional` but not `Iterable`, so its three `Hyper` candidates
tie), and `$carray >>*>> 2` is the elementwise `$(2e0, 4e0)`. The infix hyper
operator now resolves such an operand (an instance or a role-mixed value) before
the usual operand handling: against another listy operand, or against another
such object, it raises `X::Multi::Ambiguous`; against a plain scalar it expands
as the object's `.list`, itemized like rakudo's result.
`LLM::RetrievalAugmentedGeneration`'s `t/01` test 10 (`nok ... { sum($^a <<*>> $^b) }`)
relies on the failure and now matches `raku`.

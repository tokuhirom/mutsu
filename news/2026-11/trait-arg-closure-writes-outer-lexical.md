# A closure given as a routine trait argument writes the outer lexical itself

`sub bar($a) is mm({ ++$seen; 1 })` evaluates its trait argument when the hoisted
declaration is registered, before the enclosing `my $seen = 0` has run, so the
closure updated a copy of the lexical. The compiler now boxes the outer
lexicals a custom-trait argument writes into shared cells at their declaration,
as it already does for a nested named sub's writes. Sub::Memoized's
`t/01-basic.rakutest` now matches rakudo (85/85, was 83/85). Closes #16326.

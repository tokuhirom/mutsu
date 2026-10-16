# min, max, sort, unique and friends with a callback are rows

`min`, `max`, `minmax`, `minpairs`, `maxpairs`, `sort`, `unique` and `repeated` called with a callback or an adverb
(`.max(&by)`, `.sort(&cmp)`, `.unique(:as(...))`), or on a receiver the pure row cannot read, were answered by
name-matching arms in the interpreter's method cascade. They are now rows: each pure row has an interpreter twin of the
same owner and arity, which the method-table walk tries when the pure row declines (ADR-11276 §9.135). `.minpairs(1)`
is `X::Multi::NoMatch`, as in Rakudo.

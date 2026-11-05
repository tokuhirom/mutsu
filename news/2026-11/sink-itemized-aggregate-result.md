# A routine returning a `$` variable that holds an aggregate is not sunk

`sub f { my $r = [1, 2] but R; $r }; f;` no longer runs `R`'s `sink` method,
as in rakudo. The rule, measured against rakudo for Array, List and Hash and
their subclasses and `does`/`but` mixins: a value that came out of a `$`
container as an itemized aggregate is not sunk, while a fresh one the routine
produces or `return`s, and an ordinary object or scalar-valued mixin held in a
variable, still is.

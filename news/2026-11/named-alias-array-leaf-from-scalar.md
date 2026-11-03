# A renamed `:offset(:@k)` parameter accepts a scalar variable holding an Array

`next-prime(33, :$k)` with `my $k = [3, 4, 9]` reaches the callee's `:offset(:@k)!`
leaf through an exported proto's `|` capture as the variable's container; the
renamed-named binder checked the container instead of the Array in it and died with
"expected Positional, got Array". Math::NumberTheory `t/04-prime` now matches rakudo
(77 of 77, was 18), as do all 19 of its test files.

use v6.d;
use Test;

# ADR-11276 §9.149 (#13738): under 6.d, `Complex.sign` is `Cool.sign`, which
# refuses a non-real `Complex`, and `Int.pick`/`Int.roll` are `Any`'s, which
# sample the one-item list `(self,)` (`revision-e-shape-method-rows.t` has
# the 6.e methods).

plan 4;

dies-ok { (1+2i).sign }, 'Cool.sign refuses a non-real Complex';
is (3+0i).sign, 1, 'Cool.sign of a real Complex is an Int';
is 5.pick, 5, 'Any.pick of an Int is the Int';
is 5.roll(3).List, (5, 5, 5), 'Any.roll of an Int rolls the Int';

use v6;
use Test;

# From Sequence::Generator: `(Any:D $a, Real:D $b)` and `(@a, Mu $b)` are each
# narrower on a different declared type (Positional < Any, Real:D < Mu), so
# rakudo calls the pair ambiguous and `is default` picks the winner.

plan 8;

class X {
    multi method f(Any:D $a, Real:D $b) { "A" }
    multi method f(@a, Mu $b) is default { "B" }
}
is X.f((1,2,4), 64), "B", 'method: the is default candidate settles the split';
is X.f(5, 64), "A", 'a scalar first argument only fits the Any:D candidate';

class Y {
    multi method f(Any:D $a, Real:D $b) is default { "A" }
    multi method f(@a, Mu $b) { "B" }
}
is Y.f((1,2,4), 64), "A", 'is default on the other candidate';

multi sub h(Any:D $a, Real:D $b) { "A" }
multi sub h(@a, Mu $b) is default { "B" }
is h((1,2,4), 64), "B", 'sub: the is default candidate settles the split';
is h(5, 64), "A", 'and a scalar still reaches the other';

class Z {
    multi method f(Any:D $a, Real:D $b) { "A" }
    multi method f(@a, Mu $b;; int :$nf) is default { "B" }
    multi method f(Real:D $a, Real:D $b;; int :$nf) { "R" }
}
is Z.f((1,2,4), 64), "B", 'with a third, named-parameter candidate';

class W {
    multi method f(Any:D $a, Real:D $b) { "A" }
    multi method f(@a, Mu $b) { "B" }
}
throws-like { W.f((1,2,4), 64) }, X::Multi::Ambiguous, 'without is default the split is ambiguous (method)';
multi sub k(Any:D $a, Real:D $b) { "A" }
multi sub k(@a, Mu $b) { "B" }
throws-like { k((1,2,4), 64) }, X::Multi::Ambiguous, 'and for a sub';

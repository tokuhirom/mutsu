use Test;

# From Math::SparseMatrix (t/26-dot-product): a `:@named! where ...` multi
# candidate must see a Seq argument as the List the `@` parameter binds.
plan 6;

class A {
    multi method m(:@d! where @d ~~ List:D) { "where" }
    multi method m(:@d!) { "fallback" }
}
my $s = (1, 2).map({ [$_,] });
is A.m(d => $s), "where", 'method multi: Seq binds as List in named where';
is A.m(d => $s.Array), "where", 'Array argument unchanged';

multi sub f(:@d! where @d.all ~~ List:D) { "sub where" }
multi sub f(:@d!) { "sub fallback" }
is f(d => $s), "sub where", 'sub multi: Seq binds as List in named where';

# Rakudo types `round(Num, Rat)` as Int * Rat, i.e. an exact Rat.
my $r = round(0.34567e0, 0.001);
isa-ok $r, Rat, 'round(Num, Rat) is a Rat';
is $r, 0.346, 'round(Num, Rat) value';
is (round(0.3456e0, 0.001) xx 3).sum, 1.038, 'sums of rounded values are exact';

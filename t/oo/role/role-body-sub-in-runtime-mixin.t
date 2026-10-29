use v6;
use Test;

# Grid: a lexical `sub` in a parametric role body is found by the role's
# methods when the role is mixed into a variable at run time
# (`@a does R[:c(1)]`), not only when a class composes it.

plan 4;

role R[Int :$c] {
    sub helper($x) { $x * 2 }
    method plain { helper(5) }
    multi method typed(R:D: Int:D :$d!) { helper($d) }
    multi method typed(R:D: :@d!) { my @s = @d; @s does R[:c(2)]; @s.typed(:d(7)) }
}

my @a = 1, 2, 3;
@a does R[:c(1)];
is @a.plain, 10, 'a plain method calls the role sub on a mixed-in array';
is @a.typed(:d(4)), 8, 'a multi method calls it too';
is @a.typed(:d[1, 2]), 14, 'a nested mixin calls it through a second composition';

class K does R[:c(3)] { }
is K.new.plain, 10, 'a class composition is unchanged';

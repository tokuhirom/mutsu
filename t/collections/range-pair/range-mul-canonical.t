use Test;

# `Range * Real` must produce a canonical integer Range so it is `eqv` to a
# literal range, matching `+`/`-` (regression: scaling returned a GenericRange
# that rendered identically but failed eqv/is-deeply).

plan 7;

is-deeply (2..5)   * 2, 4..10,    'inclusive Range * Int';
is-deeply (2..^5)  * 5, 10..^25,  'exclusive-end Range * Int';
is-deeply (2^..5)  * 2, 4^..10,   'exclusive-start Range * Int';
is-deeply (2^..^5) * 2, 4^..^10,  'exclusive-both Range * Int';
is-deeply 5 * (2..^5),  10..^25,  'Int * Range';

# role mixin is preserved across the multiplication
my role Meows {}
my $r := (2..^5) but Meows;
ok ($r * 5) ~~ Meows, 'role preserved through Range * Int';
is-deeply ($r * 5), ((10..^25) but Meows), 'mixed Range * Int eqv literal';

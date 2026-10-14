use Test;

# ADR-11276 §9.103: `Range.in-range` is the row's alone (its 1-arg cascade
# arm is gone); arguments the guard step declines reach the `ANY_ARGS` row.

plan 4;

ok (1..10).in-range(5), 'in-range($value)';
ok (1..10).in-range(<5>), 'an allomorph value';
my $x = 5;
ok (1..10).in-range($x), 'an itemized value';
throws-like { (1..3).in-range(7) }, X::OutOfRange, 'out of range throws';

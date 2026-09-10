use Test;

# `.pairs`/`.antipairs`/`.kv` over a genuinely-lazy list must stay lazy
# (not force the source). Regression pin for S02-types/lazy-lists.t 24-26.
# Also verifies the produced values match the eager equivalents.

plan 18;

my $was-lazy;
sub make-lazy-list($num) { gather { take $_ for 0 ..^ $num; $was-lazy = 0 }.lazy };

# Laziness: assigning the whole transform to an @-array must not force the
# gather body (which would run `$was-lazy = 0`).
$was-lazy = 1;
my \one = make-lazy-list(10);
my @res-kv = one.kv;
ok $was-lazy, 'kv over a lazy list is lazy';

$was-lazy = 1;
my \two = make-lazy-list(10);
my @res-pairs = two.pairs;
ok $was-lazy, 'pairs over a lazy list is lazy';

$was-lazy = 1;
my \three = make-lazy-list(10);
my @res-anti = three.antipairs;
ok $was-lazy, 'antipairs over a lazy list is lazy';

# .is-lazy on the transform result is True (both bound and chained).
my \four = make-lazy-list(4);
my $p := four.pairs;
ok $p.is-lazy, 'pairs result reports is-lazy True';
ok make-lazy-list(4).kv.is-lazy, 'chained kv reports is-lazy True';
ok make-lazy-list(4).antipairs.is-lazy, 'chained antipairs reports is-lazy True';

# Values: a bounded prefix matches the eager transform.
is make-lazy-list(4).pairs.head(3).List, (0 => 0, 1 => 1, 2 => 2),
    'lazy pairs prefix values';
is make-lazy-list(4).antipairs.head(3).List, (0 => 0, 1 => 1, 2 => 2),
    'lazy antipairs prefix values';
is make-lazy-list(4).kv.head(6).List, (0, 0, 1, 1, 2, 2),
    'lazy kv prefix values';

# Full eager realization matches the eager transform exactly.
is make-lazy-list(4).pairs.eager, (0 => 0, 1 => 1, 2 => 2, 3 => 3),
    'lazy pairs full values';
is make-lazy-list(4).antipairs.eager, (0 => 0, 1 => 1, 2 => 2, 3 => 3),
    'lazy antipairs full values';
is make-lazy-list(4).kv.eager, (0, 0, 1, 1, 2, 2, 3, 3),
    'lazy kv full values';

# Over an infinite source: stays lazy, a bounded slice still terminates.
is ((1 .. *).pairs[^3]).List, (0 => 1, 1 => 2, 2 => 3),
    'pairs over infinite range, bounded slice';
is ((1 .. *).antipairs[^3]).List, (1 => 0, 2 => 1, 3 => 2),
    'antipairs over infinite range, bounded slice';
is ((1 .. *).kv[^6]).List, (0, 1, 1, 2, 2, 3),
    'kv over infinite range, bounded slice';

# Non-lazy (eager) gather still materializes: .pairs is not lazy there.
nok (gather { take $_ for ^3 }).pairs.is-lazy,
    'pairs over a non-lazy gather is not lazy';
my @e = (gather { take $_ for ^3 }).pairs;
is @e.elems, 3, 'non-lazy gather pairs has all elements';

# antipairs values where the element is a string (positional ValuePair form).
is make-lazy-list(3).map({ "v$_" }).antipairs.eager, ("v0" => 0, "v1" => 1, "v2" => 2),
    'antipairs keeps element as key for strings';

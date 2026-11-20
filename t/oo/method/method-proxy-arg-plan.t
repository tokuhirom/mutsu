use Test;

# A Proxy argument keys the plain-method lane and its call plan by a marker
# (#18321): a `Mu $x is raw` parameter keeps the Proxy and never FETCHes it,
# a typed parameter FETCHes it once per call -- on the first call and on every
# replay.

plan 8;

my $fetches = 0;
my $stored;
my $p := Proxy.new(
    FETCH => -> $ { ++$fetches; 42 },
    STORE => -> $, $v { $stored = $v },
);

class C {
    method keep(Mu $x is raw) { 1 }
    method write(Mu $x is raw, $v) { $x = $v; 1 }
    method typed(Int $x) { $x + 1 }
    method plain($x) { $x }
}
my $c = C.new;

my $s = 0;
$s += $c.keep($p) for ^20;
is $s, 20, 'a raw Mu parameter takes a Proxy argument on every call';
is $fetches, 0, 'a raw Mu parameter never FETCHes the Proxy';

$c.write($p, $_) for ^5;
is $stored, 4, 'a write through the raw parameter STOREs into the Proxy, on every replay';
is $fetches, 0, '... without FETCHing it';

$fetches = 0;
my $t = 0;
$t += $c.typed($p) for ^20;
is $t, 43 * 20, 'a typed parameter binds what the Proxy FETCHes';
is $fetches, 20, 'a typed parameter FETCHes the Proxy once per call';

# The FETCH answer may change between calls; the plan must not pin it.
my $flip = 0;
my $q := Proxy.new(FETCH => -> $ { $flip++ %% 2 ?? 1 !! 'x' }, STORE => -> $, $ { });
is (^6).map({ $c.plain($q) }).join(','), '1,x,1,x,1,x', 'a changing FETCH answer is read afresh on every call';

# A plain value at the same position does not replay the Proxy plan.
is (^3).map({ $c.keep(7) + $c.keep($p) }).join(','), '2,2,2', 'Proxy and plain arguments alternate';

use Test;

plan 2;

# Math::NumberTheory 0.1.4 uses this shape for power-mod: a bodyless proto
# constrains definedness while a Complex multi candidate accepts another type.
proto sub power-like(Int:D $base, $exp, Int:D $mod) {*}
multi sub power-like(Int:D $base, $exp, Int:D $mod) { 'integer' }
multi sub power-like(Complex:D $base, $exp, Int:D $mod) { 'complex' }

is power-like(2, 2, 3), 'integer', 'bodyless proto dispatches to Int candidate';
is power-like(2 + 1i, 2, 3), 'complex',
    'bodyless definedness proto dispatches to Complex candidate';

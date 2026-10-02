use Test;

plan 3;

# An untyped block parameter has implicit Mu, so it takes both Mu itself and
# Junction values. The body, not the call, autothreads the addition.
is (-> $p { $p.raku })(Mu), 'Mu', 'pointy block accepts Mu';
is (-> $a, $b { $a + $b })((1|2), 3).raku, 'any(4, 5)',
    'pointy block accepts a Junction argument';
is (* + *)((1|2), 3).raku, 'any(4, 5)',
    'WhateverCode accepts a Junction argument';

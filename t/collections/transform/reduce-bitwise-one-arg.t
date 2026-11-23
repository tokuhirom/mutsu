use Test;

# The boolean bitwise operators reduce (`[?|]`, `[?&]`, `[?^]`), and a
# one-element bitwise reduction is the operator's one-argument candidate:
# `[+|] x` is `x` as it is, `[~|] x` is its Str, `[?|] x` is its Bool.

plan 14;

is ([?|] 0, 0, 1), True, '[?|]';
is ([?|] 0, 0), False, '[?|] all false';
is ([?&] 1, 1, 0), False, '[?&]';
is ([?&] 1, 1), True, '[?&] all true';
is ([?^] 1, 1, 1), True, '[?^] odd count of true';
is ([?^] 1, 1), False, '[?^] even count of true';
is-deeply [\?|](0, 0, 1, 0).List, (0, False, True, True), '[\?|] scans';

is-deeply ([?|] "0"), True, '[?|] x is x.Bool ("0" is true)';
is-deeply ([?^] 0), False, '[?^] x is x.Bool';
is-deeply ([+|] "5"), "5", '[+|] x is x unchanged';
is-deeply ([+&] "x"), "x", '[+&] x does not numify';
is-deeply ([+^] 2.5), 2.5, '[+^] x keeps a Rat';
is-deeply ([~|] 5), "5", '[~|] x is x.Str';
is-deeply ([~&] 5.5), "5.5", '[~&] x is x.Str';

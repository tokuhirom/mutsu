use Test;

plan 4;

is-deeply
    (for 1..3 { 'ﬆ' x $_ ~ 'T' ~~ /. <( [:i T]/ })».Str,
    ('T' xx 3),
    'capture markers <( ... ) affect smartmatch result';

is-deeply
    (for 1..3 { S:i/T/Z/ with 'ﬆ' x $_ ~ 'T' })».Str,
    (for 1..3 { 'ﬆ' x $_ ~ 'Z' }),
    'S:i with statement modifier returns expression values';

my $r = rx/e/;
is 'hello'.match($r), 'hello' ~~ /$r/, 'regex scalar interpolation in regex literals';

is-deeply ('T' xx 3), ('T' xx 3), 'sequence repetition deep equality';

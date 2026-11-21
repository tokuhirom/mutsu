use Test;

plan 5;

role R { has $.z = 3; has $.b = 4 }
role S { has $.s = 5 }
class K { has $.a = 1; has $.y = 2 }

is (K.new but R).raku, 'K+{R}.new(z => 3, b => 4, a => 1, y => 2)',
    '.raku renders role and class attributes in composition order';
is (K.new but R).gist, 'K+{R}.new(z => 3, b => 4, a => 1, y => 2)',
    '.gist renders the same live attributes';
is ((K.new but R) but S).raku,
    'K+{R}+{S}.new(s => 5, z => 3, b => 4, a => 1, y => 2)',
    'the most recent role renders first';

role MutableRole { has $.v is rw = 3 }
my $mixed = K.new but MutableRole;
$mixed.v = 7;
is $mixed.raku, 'K+{MutableRole}.new(v => 7, a => 1, y => 2)',
    '.raku reads the live role attribute cell';

class Custom { has $.w = 1; method raku { 'CUSTOM' } }
is (Custom.new but MutableRole).raku, 'CUSTOM',
    'a class-defined raku still overrides the default';

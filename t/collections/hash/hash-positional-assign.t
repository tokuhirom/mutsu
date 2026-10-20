use Test;

# A Hash does not do Positional: `%h[0] = ...` is the inherited
# Any.ASSIGN-POS (index 0 is the hash itself, STORE; others are out of range).
plan 8;

my %h = a => 1;
throws-like { %h[0] = 42 }, X::Hash::Store::OddNumber, '%h[0] = 42 is a hash initializer with one element';
is %h.raku, '{:a(1)}', 'the hash is untouched after the failed store';

my %g = a => 1;
%g[0] = b => 2, c => 3;
is-deeply %g, %(b => 2, c => 3), '%g[0] = pairs replaces the contents';

my %i = a => 1;
throws-like { %i[1] = 5 }, X::OutOfRange, '%i[1] = 5 is out of range';
throws-like { %i[0] := 42 }, X::Bind, '%i[0] := 42 cannot bind';
throws-like { %i[1] := 42 }, X::Bind, '%i[1] := 42 cannot bind';

my $r = { a => 1 };
throws-like { $r[0] = 42 }, X::Hash::Store::OddNumber, 'same through a scalar holding a hash';
$r[0] = b => 2;
is-deeply $r, { b => 2 }, 'a pair list stored through the scalar replaces the contents';

use Test;

# A map chained onto another map fires its LAST phaser when a read sees the
# end, and a multi-parameter grep chained onto a map takes whole groups, as
# under Rakudo (#14196). Expected values are the `raku` (v2026.09) output.

plan 11;

my ($l, $a) = 0, 0;
my $s = (1..6).map({ $a++; $_ * 2 }).map({ LAST { $l++ }; $_ + 1 });
is $s[1], 5, 'a prefix read of a chained map';
is $l, 0, '... fires no LAST';
is $s.elems, 6, 'elems reaches the end';
is "$a $l", "6 1", '... which fires LAST once, after every upstream element';

my $g = 0;
my $t = (1..10).map({ $g++; $_ }).grep(-> $x, $y { $x %% 3 });
is $t[0].raku, '(3, 4)', 'a two-parameter grep chained onto a map';
is $g, 4, '... pulled two whole groups from upstream';
is $t.elems, 2, 'elems';

my $q = 0;
my $u = (1..6).map({ $_ }).map(-> $x, $y { LAST { $q++ }; $x + $y });
is $u[0], 3, 'a chained two-parameter map';
is $q, 0, '... has not fired LAST';
is $u.elems, 3, 'elems';
is $q, 1, "... which fires LAST once";

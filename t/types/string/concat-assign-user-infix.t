use Test;

# `$x ~= $y` asks the user `infix:<~>` candidates, as `$x ~ $y` does (#13791).
# Checked against rakudo.
plan 6;

class V { has $.s }
multi sub infix:<~>(V $a, V $b) { V.new(s => $a.s ~ '+' ~ $b.s) }

my $d = V.new(s => 'x');
$d ~= V.new(s => 'y');
is $d.s, 'x+y', 'a local';

my @a = V.new(s => 'a');
@a[0] ~= V.new(s => 'b');
is @a[0].s, 'a+b', 'an element';

our $o = V.new(s => 'o');
$o ~= V.new(s => 'p');
is $o.s, 'o+p', 'a package variable';

my &c = { $d ~= V.new(s => 'z') };
c();
is $d.s, 'x+y+z', 'a captured variable';

my $s = 'q';
$s ~= 'r';
is $s, 'qr', 'two strings still concatenate';

is (('a' | 'b') ~ 'c').Str, any('ac', 'bc').Str, 'a Junction operand threads';

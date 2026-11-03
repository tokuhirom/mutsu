use Test;

# A `my atomicint` anywhere in the process used to switch the light closure
# call off for every closure (#9170). The closure answers must not change, and
# a closure whose own parameter shares the atomic name must still read it as
# the plain parameter it is.

plan 8;

my atomicint $cnt = 0;
my $inc = -> $x { $x + 1 };
my $sum = 0;
for ^200 { $sum += $inc($_); $cnt⚛++ }
is $sum, 20100, 'a plain closure called in a loop beside an atomic variable';
is $cnt, 200, 'the atomic counter is intact';

my $shadow = -> $cnt { $cnt + 1 };
is $shadow(5), 6, 'a parameter named like the atomic variable is the argument';
is $cnt, 200, '... and does not touch the atomic variable';

my $wc = * + 1;
is (^3).map($wc).list.raku, '(1, 2, 3)', 'a WhateverCode beside an atomic variable';

my $ph = { $^a * $^b };
is $ph(3, 4), 12, 'a placeholder block beside an atomic variable';

my $mixed = -> $a, $b { $a ~ $b };
is $mixed('x', 'y'), 'xy', 'two parameters';

my @threads = (^4).map: -> $t { start { my $f = -> $y { $y * 2 }; (^50).map({ $f($_) }).sum } };
is-deeply @threads.map({ await $_ }).List, (2450, 2450, 2450, 2450), 'closure calls from several threads';

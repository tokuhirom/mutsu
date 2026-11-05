use Test;

# #15922: a CORE routine value (`&CORE::infix:<op>`, `CORE::<&infix:<op>>`)
# runs the built-in operator and ignores the program's own candidates of the
# same name; the operator spelled in the source still sees them.

plan 8;

class Box { has $.v; method Str { ~$!v } }
multi sub infix:<eq>(Box $a, Box $b) { True }
multi sub infix:<leg>(Box $a, Box $b) { Less }
multi sub infix:<lt>(Box $a, Box $b) { True }

my $x = Box.new(:v(2));
my $y = Box.new(:v(1));

nok CORE::<&infix:<eq>>.($x, $y), 'CORE::<&infix:<eq>> ignores the user eq';
nok &CORE::infix:<eq>($x, $y), '&CORE::infix:<eq> ignores the user eq';
is CORE::<&infix:<leg>>.($x, $y), More, 'CORE leg compares the strings';
nok CORE::<&infix:<lt>>.($x, $y), 'CORE lt compares the strings';

ok ($x eq $y), 'the operator in source still sees the user eq';
is ($x leg $y), Less, 'and the user leg';
ok ($x lt $y), 'and the user lt';
ok CORE::<&infix:<eq>>.("a", "a"), 'the CORE routine still compares strings';

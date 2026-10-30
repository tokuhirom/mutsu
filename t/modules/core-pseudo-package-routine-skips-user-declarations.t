use Test;

# #15168: `&CORE::name` and `CORE::<&name>` name the core routine, so a routine
# or operator candidate the program declares under the same name must not
# answer for them.
plan 12;

class Foo { has $.x }
multi sub infix:<eqv>(Foo $a, Foo $b) { "overridden" }
multi sub infix:<+>(Foo $a, Foo $b) { "plus" }
multi sub infix:<cmp>(Foo $a, Foo $b) { "cmp" }
sub abs($x) { "mine" }

my ($a, $b) = Foo.new(x => 1), Foo.new(x => 1);
is $a eqv $b, 'overridden', 'the operator form still reaches the user candidate';
is CORE::<&infix:<eqv>>($a, $b), True, 'CORE::<&infix:<eqv>> is the core operator';
is &CORE::infix:<eqv>($a, $b), True, '&CORE::infix:<eqv> too';
my &core-eqv = &CORE::infix:<eqv>;
is core-eqv($a, $b), True, 'a variable bound to it keeps calling the core operator';
is $a + $b, 'plus', 'user + candidate still answers its own signature';
is CORE::<&infix:<+>>(1, 2), 3, 'the core + answers Int operands';
is &CORE::infix:<cmp>(1, 2), Less, 'and the core cmp';
is abs(-3), 'mine', 'the user routine answers a plain call';
is &CORE::abs(-3), 3, '&CORE::abs is the core routine';
is CORE::<&abs>(-3), 3, 'so is CORE::<&abs>';
is &SETTING::abs(-4), 4, 'SETTING:: names the core setting as well';

# A user override that delegates to the core operator does not recurse into itself.
class Bar { has $.x }
multi sub infix:<eqv>(Bar $a, Bar $b) { CORE::<&infix:<eqv>>($a, $b) }
lives-ok { Bar.new eqv Bar.new }, 'delegating to the core operator terminates';

use v6;
use Test;

plan 10;

# The MOP surface JSON::Unmarshal's ClassLike subset needs.
class Dog { has Str $.name }

ok Dog.HOW.archetypes.nominal, 'a class archetype is nominal';
nok Dog.HOW.archetypes.nominalizable, 'a class is not nominalizable';
role R {}
ok R.HOW.archetypes.composable, 'a role archetype is composable';
subset S of Str where { .chars > 1 };
ok S.HOW.archetypes.nominalizable, 'a subset is nominalizable';
nok S.HOW.archetypes.nominal, 'a subset is not nominal';

# SetHash.set / .unset mutate in place.
my SetHash $used .= new;
$used.set('a');
$used.set('b');
ok $used<a> && $used<b>, 'SetHash.set adds keys';
$used.unset('a');
nok $used<a>, 'SetHash.unset removes a key';

# Binding a Positional type object to an @-var keeps .of.
my @x := Positional[Dog];
is @x.of.^name, 'Dog', '@-bound Positional[T] exposes .of';

# ... and through an @-sigiled parameter.
sub g(@p) { @p.of.^name }
is g(Positional[Dog]), 'Dog', '@-param bound to Positional[T] exposes .of';

# The whole unmarshal-like flow: a nested typed collection dispatch.
multi um($j, @x) { "els-" ~ @x.of.^name }
multi um(Any:D $j, Mu) { "fallback" }
my @vals = ({ n => 1 },);
is um(@vals, Positional[Dog]), 'els-Dog',
    'the @-candidate out-narrows the Any:D+Mu fallback';

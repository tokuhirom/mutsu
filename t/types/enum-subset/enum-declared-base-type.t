use v6;
use Test;

# `our Str enum S «:A<a-val>»` — the base type the enum's VALUES carry is part
# of the enum's identity: its values ARE `Str`s, they are not `Int`s, and string
# context gives the value while `.gist` keeps the key. mutsu hardcoded `Int` as
# every enum's base, so `sub f(Str $x)` rejected such a value ("expected Str but
# got Int") and `~S::A` answered the key.
#
# From CSS::Grammar::Defs, which declares every CSS selector/property type as a
# `Str enum` and passes the values to `Str $type` parameters.

plan 14;

our Str enum Sel « :Alpha<a-val> :Beta<b-val> »;
enum IntE <X Y>;

ok Sel::Alpha ~~ Str,   'a Str-enum value is a Str';
ok Sel::Alpha ~~ Cool,  'a Str-enum value is Cool';
nok Sel::Alpha ~~ Int,  'a Str-enum value is not an Int';
nok Sel::Alpha ~~ Real, 'a Str-enum value is not Real';
ok Sel::Alpha ~~ Sel,   'a Str-enum value is still its own enum type';

ok IntE::X ~~ Int,  'an ordinary enum value is an Int';
nok IntE::X ~~ Str, 'an ordinary enum value is not a Str';

sub positional(Str $x) { $x }
sub named(Str :$x) { $x }
is positional(Sel::Alpha), 'a-val', 'a Str-enum value binds to a positional Str parameter';
is named(:x(Sel::Beta)), 'b-val', 'a Str-enum value binds to a named Str parameter';

is Sel::Alpha.Str, 'a-val', '.Str is the value';
is ~Sel::Alpha, 'a-val', 'string context is the value';
my $v = Sel::Alpha;
is "$v", 'a-val', 'interpolation is the value';
is Sel::Alpha.gist, 'Alpha', '.gist is still the key';
is IntE::Y.Str, 'Y', 'an ordinary enum still stringifies to its key';

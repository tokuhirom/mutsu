use Test;

# From the SeqSplitter distribution: `.^attributes.first('$!name')` smartmatches
# a Str against an Attribute by its name, and set_value writes through.

plan 3;

class P { has $.parent; has $.x = 1 }
my \c = P.new;
my $at = c.^attributes.first('$!parent');
is $at.name, '$!parent', 'first with a Str matcher finds the attribute';
$at.set_value(c, P.new(x => 7));
is c.parent.x, 7, 'set_value on a sigilless object';
ok !c.^attributes.first('$!nope').defined, 'no such attribute';

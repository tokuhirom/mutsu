use Test;

# A variable trait that mixes a role in via `trait_mod:<does>` on a variable a
# routine captures composes onto the value its shared cell holds; wrapping the
# cell itself stored a cycle that deadlocked the next read.

plan 3;

my role Tagged { method tag { 'tagged' } }
multi sub trait_mod:<is>(Variable:D \v, :$tagged!) { trait_mod:<does>(v, Tagged) }

my %h is tagged = a => 1;
sub peek() { %h.tag }
is peek(), 'tagged', 'captured hash keeps the mixed-in role';
is %h<a>, 1, 'contents survive';
ok %h.^name.contains('Tagged'), 'type name shows the role';

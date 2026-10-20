use v6;
use Test;

# `Collation`, `Attribute` and `Variable` instances reach their method rows
# through dispatch shapes, from every entry including the mutating by-name one
# (ADR-11276, slice 5).

plan 7;

my $c = $*COLLATION;
$c.set(:tertiary(0));
is $c.tertiary, 0, 'Collation.set through a variable writes the shared cell';
is $c.primary, 1, 'the other levels are untouched';
is $*COLLATION.tertiary, 0, 'every alias sees it';
$c.set(:tertiary(1));

class A { has $.x is rw = 5 }
my $attr = A.^attributes[0];
is $attr.name, '$!x', 'Attribute.name';
is $attr.get_value(A.new), 5, 'Attribute.get_value takes a user object';
my $o = A.new;
$attr.set_value($o, 9);
is $o.x, 9, 'Attribute.set_value takes a user object';
is $attr.rw, True, 'Attribute.rw';

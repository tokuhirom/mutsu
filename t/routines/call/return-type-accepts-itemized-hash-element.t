use v6;
use Test;

# A Bool stored into a real Hash element is itemized; returning it from a
# routine with a `--> Bool` return type must check what the item holds, as a
# `Bool $x` parameter does (EEL's `EEL::Runner::nullable` returned
# `%nullable{$name} // False`).

plan 10;

my %n = x => False, y => True;

sub direct(%h, $k --> Bool) { %h{$k} }
is direct(%n, 'x'), False, 'a hash element holding False passes --> Bool';
is direct(%n, 'y'), True, 'a hash element holding True passes --> Bool';

sub defaulted($k, %h --> Bool) { $k ne 'event' && (%h{$k} // False) }
is defaulted('x', %n), False, 'an element through && and // passes --> Bool';
is defaulted('z', %n), False, 'the // default passes --> Bool';

sub explicit(%h --> Bool) { return %h<y> }
is explicit(%n), True, 'an explicit return of the element passes --> Bool';

sub all-of(%h --> Bool) { so <x y>.map({ defaulted($_, %h) }).all }
is all-of(%n), False, 'a junction over such calls collapses to Bool';

# Bool is-an Int, so it satisfies an Int return type too.
sub as-int(%h --> Int) { %h<x> }
is as-int(%n), False, 'a Bool element passes --> Int';
sub type-int(--> Int) { Bool }
is type-int().^name, 'Bool', 'the Bool type object passes --> Int';
my %s = x => 'str';
sub wrong(%h --> Int) { %h<x> }
throws-like { wrong(%s) }, X::TypeCheck::Return, 'a Str element still fails --> Int';

my @a = False;
sub from-array(@a --> Bool) { @a[0] }
is from-array(@a), False, 'an array element passes --> Bool';

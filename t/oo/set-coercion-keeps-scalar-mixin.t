use v6;
use Test;

# `Set()` / `.Set` on a scalar object that had a role mixed in keeps that
# object as the single element, mixin included: only an aggregate's mixin is
# folded into its elements. Found by RedX::HashedPassword (an Attribute with
# Red::Attr::Column mixed in lost the role through `Set() $attr`).

plan 5;

role Col { method column { "colmeth" } }
class C { has Str $.t; }

my $attr = C.^attributes[0];
$attr does Col;

sub coerced(Set() $s) { $s.keys[0].column }
is coerced($attr), 'colmeth', 'a Set() parameter keeps the mixin';
is $attr.Set.keys[0].column, 'colmeth', '.Set keeps the mixin';
is $attr.Bag.keys[0].column, 'colmeth', '.Bag keeps the mixin';
is Set($attr).keys[0].column, 'colmeth', 'Set(...) keeps the mixin';

my %h = a => 1;
my %mixed = %h but Col;
is %mixed.Set.keys.sort.List, ('a',), 'an aggregate mixin still folds into its elements';

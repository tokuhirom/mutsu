use v6;
use Test;

plan 6;

# `--> Mu` takes every value, a type object of any class included. The light
# call path compared a returned type object to the declared return type by
# name, so `sub (Mu $n --> Mu) { $n.parent }` died when `parent` was unset
# (#15092, Selkie's `next-ancestor`).
class Bt { has Bt $.parent is rw; }
my $b = Bt.new;

sub parent-of(Mu $node, int $hops --> Mu) { $node.parent }
sub the-class(Mu $n --> Mu) { Bt }
sub an-int(Mu $n --> Mu) { Int }
sub the-mu(Mu $n --> Mu) { Mu }
sub any-bt(Mu $n --> Any) { Bt }

is parent-of($b, 0).^name, 'Bt', 'an unset attribute of a class type returns that type object';
is the-class($b).^name, 'Bt', 'a class type object satisfies Mu';
is an-int($b).^name, 'Int', 'another type object satisfies Mu';
is the-mu($b).^name, 'Mu', 'the Mu type object satisfies Mu';
is any-bt($b).^name, 'Bt', 'a class type object satisfies Any';

sub any-mu(Mu $n --> Any) { Mu }
throws-like { any-mu($b) }, X::TypeCheck::Return, 'the Mu type object still does not satisfy Any';

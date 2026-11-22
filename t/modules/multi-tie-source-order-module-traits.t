use Test;
use lib 't/lib';
use TieOrderRoleFirst;
use TieOrderRoleLast;
use TieOrderUser;

# Multi candidates that tie on every ranking key but declaration order are
# tried in SOURCE order, also when they are exported trait_mod candidates of a
# module that another loaded module (TieOrderUser) uses too (#18046: Red's
# `(Red::Attr::Column $attr, :%column!)` vs `(Attribute $attr, :%column!)`).
# Expected values are rakudo's.
plan 4;

{
    my $a = Attribute.new(:name<$!x>, :type(Str), :package(Any));
    trait_mod:<is>($a, :tie-a);
    trait_mod:<is>($a, :tie-b);
    is-deeply $a.args, {:a, :b}, 'role candidate declared first wins the tie';
}

{
    my $a = Attribute.new(:name<$!y>, :type(Str), :package(Any));
    trait_mod:<is>($a, :tie-c);
    trait_mod:<is>($a, :tie-d);
    is-deeply $a.args, {:d}, 'Attribute candidate declared first wins the tie';
}

is-deeply TieOrderUser.^attributes.first(*.name eq '$!x').args, {:a, :b},
    'traits applied in a loading module follow source order';
is-deeply TieOrderUser.^attributes.first(*.name eq '$!y').args, {:d},
    '...in either declaration order';

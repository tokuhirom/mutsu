use v6;
use Test;

plan 4;

# An unsupplied optional `&b?` role parameter is the `Callable` type object in
# the role body also when the role is mixed in place (`$obj does R[...]`), so
# the body can pass it on to a parameterisation of the role (Red's
# `Red::Attr::Relationship.transfer`). It read as Nil after #17766 and the
# re-parameterisation found no candidate.
role R[&a, &b?, Str :$model] {
    method n { &b.^name }
    method transfer() {
        Attribute.new(:name<$!y>, :type(Int), :package(Any)) but R[&a, &b, :$model]
    }
}

my $attr = Attribute.new(:name<$!x>, :type(Int), :package(Any));
$attr does R[{ 1 }, :model<M>];
is $attr.n, 'Callable', 'the unsupplied callable is Callable after does';
is $attr.transfer.n, 'Callable', 'and passes on to a new parameterisation';

class K {}
my $k = K.new;
$k does R[{ 1 }];
is $k.n, 'Callable', 'on a plain instance too';
is (K.new but R[{ 1 }, { 2 }]).n, 'Block', 'a supplied one is the block';

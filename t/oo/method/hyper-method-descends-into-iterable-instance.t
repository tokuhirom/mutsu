use Test;

# `>>.method` over a list whose elements are instances of a class that composes
# `Iterable` descends through each instance's `iterator`, as it does for a nested
# List (Red: `$adam.kids>>.kids>>.value` over `ResultSeq`s).

plan 3;

role MySeq does Sequence does Positional {
    has @.items;
    method iterator { @!items.iterator }
}
my $rs = Metamodel::ClassHOW.new_type(:name<HyperRS>);
$rs.^add_role(MySeq);
$rs.^add_role(Iterable);
$rs.^compose;

class Item { has $.v; method v2 { $!v } }
class Node {
    has $.cls;
    method kids { $!cls.new(items => (Item.new(:v<x>), Item.new(:v<y>))) }
}

my @nodes = Node.new(cls => $rs), Node.new(cls => $rs);
is-deeply (@nodes>>.kids)>>.v2, [<x y>, <x y>], 'the method reaches the elements of each Iterable instance';
is-deeply @nodes>>.kids>>.v2, [<x y>, <x y>], 'chained hypers';
is-deeply (@nodes>>.kids).map(*.elems).List, (2, 2), 'a non-descending method still sees the instance';

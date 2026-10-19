use Test;

plan 10;

module AliasConstraint::Tree {
    our class Node { method value { 42 } }
    our role Leaf { }
}
constant VT = AliasConstraint::Tree;

my VT::Node $node = VT::Node.new;
is $node.value, 42, 'a nested class through a constant qualifies a scalar';

sub read-node(VT::Node:D $arg --> VT::Node:D) { $arg }
is read-node($node).value, 42, 'parameter and return constraints follow the package alias';
ok $node ~~ VT::Node, 'smartmatch agrees with the aliased constraint';
dies-ok { read-node(42) }, 'the parameter rejects an unrelated value';
dies-ok { $node = 42 }, 'later stores retain the nested class constraint';

my VT::Node $empty;
ok $empty === AliasConstraint::Tree::Node, 'the default is the nested type object';

my VT::Node @nodes = $node;
is @nodes[0].value, 42, 'typed array elements use the same alias resolution';
dies-ok { @nodes.push(42) }, 'typed array writes reject an unrelated value';

class Twig does VT::Leaf { }
sub read-leaf(VT::Leaf $arg) { $arg }
ok read-leaf(Twig.new) ~~ AliasConstraint::Tree::Leaf,
    'a nested role qualifies a parameter through the alias';

constant Again = VT;
my Again::Node:D $again = $node;
is $again.value, 42, 'an alias of the package alias preserves definedness';

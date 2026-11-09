use v6.e.PREVIEW;
use Test;

# `sorries`, `worries`, `has-check-time-problems`, `add-sorry` and `add-worry`
# belong to the `CheckTime` role every node composes (and to `CompUnit`), so every
# node `can` them (#14916); `clone` copies a node.

plan 12;

my $cu = 'my $x = 1 + 2; say $x; 5'.AST;
my @nodes = $cu, $cu.statements[0], $cu.statements[0].expression, $cu.statements[2].expression;

for <sorries worries has-check-time-problems add-sorry add-worry clear-worries promote-worries-to-sorries> -> $m {
    ok @nodes.map({ .can($m).so }).all, "every node can $m";
}

is-deeply @nodes.map({ .sorries.elems }).List, (0, 0, 0, 0), 'a parsed tree has no sorries';

# `clone` copies the node and shares its children.
my $copy = $cu.clone;
is $copy.^name, 'RakuAST::StatementList', 'a cloned node has the class of the original';
nok $copy === $cu, 'it is a different object';
is $copy.raku, $cu.raku, 'it prints the same';
ok $copy.statements[0] === $cu.statements[0], 'its children are shared';

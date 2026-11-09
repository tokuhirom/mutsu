use Test;

# `nodemap` on a value that is not a list or hash is a one-element List:
# the callback's result, wrapped (also for a RakuAST node).
plan 7;

is 5.nodemap(*+1).raku, '(6,)', 'an Int gives a one-element list';
is "a".nodemap(*~"b").raku, '("ab",)', 'a Str gives a one-element list';
is 5.nodemap(*+1).^name, 'List', 'the result is a List';
is (1, 2).nodemap(*+1).raku, '(2, 3)', 'a list is still mapped element by element';
is 5.nodemap(-> $x { Empty }).raku, '(Empty,)', 'Empty is an element, not a slip';

use experimental :rakuast;
my $ast = 'say 1'.AST;
is $ast.nodemap(-> $n { $n.^name }).raku, '("RakuAST::StatementList",)', 'a RakuAST node maps itself';
is RakuAST::IntLiteral.new(3).nodemap(-> $n { 1 }).^name, 'List', 'a leaf node gives a List';

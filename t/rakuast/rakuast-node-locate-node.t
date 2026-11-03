use Test;
use experimental :rakuast;

# `RakuAST::Node.locate-node($pos, $to?)` (#14916): the deepest descendant
# whose origin covers a position, or the half-open range `$pos .. $to`, found
# through `visit-children`; `Nil` outside the node. Expected values measured
# on rakudo 2026.09.

plan 20;

my $src = Q[say 1 + 2; my $x = "ab" ~ abs(3);];
my $ast = $src.AST;

sub show($node) {
    $node.defined
        ?? $node.^name.subst('RakuAST::', '') ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-')
        !! 'Nil'
}

is show($ast.locate-node(0)),  'Name 0..3', 'the first character of a name';
is show($ast.locate-node(2)),  'Name 0..3', 'the last character of a name';
is show($ast.locate-node(3)),  'Call::Name::WithoutParentheses 0..9', 'a blank between a name and its arguments';
is show($ast.locate-node(4)),  'IntLiteral 4..5', 'a literal';
is show($ast.locate-node(5)),  'ArgList 4..9', 'a blank inside an argument list';
is show($ast.locate-node(6)),  'Infix 6..7', 'an infix';
is show($ast.locate-node(8)),  'IntLiteral 8..9', 'the right operand';
is show($ast.locate-node(9)),  'StatementList 0..33', 'the semicolon belongs to the list';
is show($ast.locate-node(11)), 'VarDeclaration::Simple 11..32', 'a declaration';
is show($ast.locate-node(17)), 'Initializer::Assign 17..32', 'an initializer';
is show($ast.locate-node(20)), 'StrLiteral 20..22', 'inside a string';
is show($ast.locate-node(30)), 'IntLiteral 30..31', 'inside a call';
is show($ast.locate-node(33)), 'Nil', 'the end of the source is outside every node';

is show($ast.locate-node(4, 9)),   'ArgList 4..9',  'a range that fits a node exactly';
is show($ast.locate-node(4, 5)),   'IntLiteral 4..5', 'a range of one character';
is show($ast.locate-node(0, 100)), 'Nil', 'a range longer than the node';
is show($ast.locate-node(15, 19)), 'VarDeclaration::Simple 11..32', 'a range no child covers';
is show($ast.locate-node(5, 4)),   'ArgList 4..9', 'a reversed range stands for its start';

my $statement = $ast.statements[1];
is show($statement.locate-node(30)), 'IntLiteral 30..31', 'from a statement';
is show($statement.locate-node(2)),  'Nil', 'a position outside the node it is asked of';

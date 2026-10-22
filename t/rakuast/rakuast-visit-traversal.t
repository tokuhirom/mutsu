use Test;
use experimental :rakuast;

plan 11;

my $tree = 'say 1, 2; say 3'.AST;
my @breadth;
my $breadth-result = $tree.visit(-> $node {
    @breadth.push($node.^name);
    True
});
is-deeply @breadth, [
    'RakuAST::StatementList',
    'RakuAST::Statement::Expression', 'RakuAST::Statement::Expression',
    'RakuAST::Call::Name::WithoutParentheses', 'RakuAST::Call::Name::WithoutParentheses',
    'RakuAST::Name', 'RakuAST::ArgList', 'RakuAST::Name', 'RakuAST::ArgList',
    'RakuAST::IntLiteral', 'RakuAST::IntLiteral', 'RakuAST::IntLiteral',
], 'visit walks breadth first';
is $breadth-result.^name, 'Any', 'visit returns an undefined Any';

my @depth;
$tree.visit-dfs(-> $node {
    @depth.push($node.^name);
    True
});
is-deeply @depth, [
    'RakuAST::StatementList',
    'RakuAST::Statement::Expression',
    'RakuAST::Call::Name::WithoutParentheses', 'RakuAST::Name', 'RakuAST::ArgList',
    'RakuAST::IntLiteral', 'RakuAST::IntLiteral',
    'RakuAST::Statement::Expression',
    'RakuAST::Call::Name::WithoutParentheses', 'RakuAST::Name', 'RakuAST::ArgList',
    'RakuAST::IntLiteral',
], 'visit-dfs walks depth first';

my @strict-breadth;
$tree.visit(-> $node { @strict-breadth.push($node.^name); True }, :strict);
is-deeply @strict-breadth, @breadth[1..*].Array, 'strict breadth-first visit skips the root';
my @strict-depth;
$tree.visit-dfs(-> $node { @strict-depth.push($node.^name); True }, :strict);
is-deeply @strict-depth, @depth[1..*].Array, 'strict depth-first visit skips the root';

my @pruned;
$tree.visit(-> $node { @pruned.push($node.^name); False });
is-deeply @pruned, ['RakuAST::StatementList'], 'a false callback prunes descendants';
my @strict-pruned;
$tree.visit-dfs(-> $node { @strict-pruned.push($node.^name); False }, :strict);
is-deeply @strict-pruned,
    ['RakuAST::Statement::Expression', 'RakuAST::Statement::Expression'],
    'strict traversal prunes below each direct child';

ok $tree.^can('visit'), 'visit is an introspectable method';
ok $tree.^can('visit-dfs'), 'visit-dfs is an introspectable method';

my class NumberNode is RakuAST::IntLiteral {
    method new() { callwith(42) }
}
my $number = NumberNode.new;
my @from-subclass;
$number.visit(-> $node { @from-subclass.push($node.^name); True });
is-deeply @from-subclass, ['NumberNode'], 'visit accepts a subclass root';
my $expression = RakuAST::Statement::Expression.new(expression => $number);
my @nested-subclass;
$expression.visit-dfs(-> $node { @nested-subclass.push($node.^name); True });
is-deeply @nested-subclass,
    ['RakuAST::Statement::Expression', 'NumberNode'],
    'visit-dfs preserves a subclass child';

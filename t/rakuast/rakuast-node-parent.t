use Test;
use experimental :rakuast;

# `RakuAST::Node.parent($generation)` answers from the lineage that `map`,
# `grep` and `first` keep while they call back (rakudo's `@*LINEAGE`): the
# visited node's ancestors, nearest first, without the node the walk started
# from. It is `Nil` beyond the top and outside those walks (#14916). Expected
# values are rakudo 2026.09's.

plan 13;

my $ast = Q[say 1 + 2; say 3].AST;

my @gen;
$ast.map(-> $n {
    if $n ~~ RakuAST::IntLiteral && $n.value == 1 {
        @gen.push: (try $n.parent($_).^name) // 'ERR' for 0..6;
    }
    Empty
});
is @gen.join(' '),
    'RakuAST::ArgList RakuAST::ApplyInfix RakuAST::ArgList RakuAST::Call::Name::WithoutParentheses RakuAST::Statement::Expression Nil Nil',
    'pre-order: the lineage of an operand, nearest ancestor first, then Nil';

my @post;
$ast.map(-> $n {
    if $n ~~ RakuAST::ApplyInfix {
        @post.push: (try $n.parent($_).^name) // 'ERR' for 0..4;
    }
    Empty
}, :depth-first);
is @post.join(' '),
    'RakuAST::ArgList RakuAST::Call::Name::WithoutParentheses RakuAST::Statement::Expression Nil Nil',
    'post-order: the same lineage while the node is left';

my @default;
$ast.map(-> $n { @default.push: $n.parent.^name if $n ~~ RakuAST::IntLiteral; Empty });
is @default.join(' '), 'RakuAST::ArgList RakuAST::ArgList RakuAST::ArgList', 'parent with no argument is the nearest ancestor';

my @top;
$ast.map(-> $n { @top.push: $n.parent.^name if $n ~~ RakuAST::Statement::Expression; Empty });
is @top.join(' '), 'Nil Nil', 'a top-level statement has no parent: the walk root is not in the lineage';

my $in-grep;
$ast.grep(-> $n { $in-grep = $n.parent.^name if $n ~~ RakuAST::IntLiteral && $n.value == 3; False });
is $in-grep, 'RakuAST::ArgList', 'grep keeps the lineage';

my $in-first;
$ast.first(-> $n { $in-first = $n.parent.^name if $n ~~ RakuAST::IntLiteral && $n.value == 3; False });
is $in-first, 'RakuAST::ArgList', 'first keeps the lineage';

my @in-find;
$ast.find-nodes(RakuAST::IntLiteral, :condition(-> $n { @in-find.push: $n.parent.^name; True }));
is @in-find.join(' '), 'Nil Nil Nil', 'find-nodes keeps no lineage';

my $int = $ast.find-nodes(RakuAST::IntLiteral)[0];
is $int.parent.^name, 'Nil', 'parent outside a walk is Nil';
nok $int.parent.defined, 'and undefined';

# A walk inside a callback has a lineage of its own and gives the outer one back.
my @nested;
$ast.map(-> $n {
    if $n ~~ RakuAST::ApplyInfix {
        my $outer = $n.parent.^name;
        $n.map(-> $m { Empty });
        @nested.push: $outer, $n.parent.^name;
    }
    Empty
});
is @nested.join(' '), 'RakuAST::ArgList RakuAST::ArgList', 'the lineage is restored after a nested walk';

# A callback called from another sub sees it too.
sub parent-name($n) { $n.parent.^name }
my @via-sub;
$ast.map(-> $n { @via-sub.push: parent-name($n) if $n ~~ RakuAST::Infix; Empty });
is @via-sub.join(' '), 'RakuAST::ApplyInfix', 'a helper sub called from the mapper sees the lineage';

my @deep;
$ast.map(-> $n { @deep.push: $n.parent(9).^name if $n ~~ RakuAST::Infix; Empty });
is @deep.join(' '), 'Nil', 'a generation beyond the top is Nil';

my $count = 0;
$ast.map(-> $n { $count++ if $n.parent.defined; Empty });
is $count, 12, 'every node but the two top-level statements has a parent';

done-testing;

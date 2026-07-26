use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 4 slice 5 (ADR-0011): mutable StatementList construction.
# This file passes under both mutsu and raku.

plan 8;

my $list = RakuAST::StatementList.new;
my $alias = $list;
is $list.gist, 'RakuAST::StatementList.new()', 'empty StatementList constructor';
is $list.statements.elems, 0, 'empty StatementList has no children';

my $first = RakuAST::Statement::Expression.new(
    expression => RakuAST::IntLiteral.new(40),
);
is $list.add-statement($first), $first, 'add-statement returns the added node';
is $list.statements.elems, 1, 'add-statement mutates the shared list';
is $alias.statements.elems, 1, 'aliases observe the same mutable node';

my $second = RakuAST::Statement::Expression.new(
    expression => RakuAST::ApplyInfix.new(
        left  => RakuAST::IntLiteral.new(40),
        infix => RakuAST::Infix.new("+"),
        right => RakuAST::IntLiteral.new(2),
    ),
);
$list.add-statement($second);
is $list.statements[1].expression.right.value, 2,
    'multiple constructed statements remain queryable';
is EVAL($list), 42, 'a constructed StatementList lowers through the existing compiler';

my @local-methods = RakuAST::StatementList.^methods(:local)>>.name.sort;
ok 'new' (elem) @local-methods && 'add-statement' (elem) @local-methods,
    'type introspection exposes construction and mutation';

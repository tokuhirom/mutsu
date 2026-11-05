use Test;

# #14916 (accessor survey): `RakuAST::Block.replace-body($new-body)` swaps the
# Blockoid of the node in place (the node is shared by every holder), answers
# `Any`, and is inherited by the pointy block and the routines, as on rakudo
# 2026.09.

plan 10;

sub blockoid($n) {
    RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new($n))))
}

my $b = RakuAST::Block.new(body => blockoid(1));
my $new = blockoid(2);
my $ret = $b.replace-body($new);
ok !$ret.defined, 'replace-body answers an undefined value';
ok $b.body === $new, 'the body is the new Blockoid';
is $b.DEPARSE.trim.subst(/\s+/, ' ', :g), '{ 2 }', 'the block deparses with the new body';
is EVAL($b), 2, 'and EVAL runs it';

my $alias = $b;
$alias.replace-body(blockoid(3));
is $b.DEPARSE.trim.subst(/\s+/, ' ', :g), '{ 3 }', 'every holder of the node sees the change';

my $p = RakuAST::PointyBlock.new(signature => RakuAST::Signature.new, body => blockoid(1));
$p.replace-body(RakuAST::Blockoid.new(RakuAST::StatementList.new));
is $p.DEPARSE, '-> { }', 'a pointy block inherits replace-body';

my $s = RakuAST::Sub.new(name => RakuAST::Name.from-identifier('f'), body => blockoid(1));
$s.replace-body(blockoid(4));
is $s.DEPARSE.trim.subst(/\s+/, ' ', :g), 'sub f { 4 }', 'a routine inherits it too';

throws-like { $b.replace-body(5) }, X::TypeCheck::Binding::Parameter,
    'a non-node is rejected';
throws-like { $b.replace-body(RakuAST::StatementList.new) }, X::TypeCheck::Binding::Parameter,
    'so is a node that is not a Blockoid';
is $b.DEPARSE.trim.subst(/\s+/, ' ', :g), '{ 3 }', 'a rejected replacement leaves the body alone';

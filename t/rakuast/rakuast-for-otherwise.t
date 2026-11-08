use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

plan 14;

my $for = Q|my @a; for @a { 1 }|.AST.statements[1];
isa-ok $for, RakuAST::Statement::For, 'parsed for is a Statement::For';
ok $for.^can('replace-otherwise').so, 'replace-otherwise is a method';
ok $for.^can('otherwise').so, 'otherwise is a method';
is $for.otherwise.^name, 'RakuAST::Block', 'default otherwise is the Block type object';
nok $for.otherwise.defined, 'default otherwise is undefined';
nok $for.raku.contains('otherwise'), '.raku leaves out otherwise';

my $pointy = RakuAST::PointyBlock.new(
    body => RakuAST::Blockoid.new(RakuAST::StatementList.new));
my $before = $for.raku;
my $r = $for.replace-otherwise($pointy);
nok $r.defined, 'replace-otherwise answers Nil';
is $for.otherwise.^name, 'RakuAST::PointyBlock', 'otherwise is the replacement';
ok $for.otherwise === $pointy, 'same object is answered';
is $for.raku, $before, '.raku is unchanged by replace-otherwise';

my $blk = RakuAST::Block.new(
    body => RakuAST::Blockoid.new(RakuAST::StatementList.new));
my $made = RakuAST::Statement::For.new(
    source => RakuAST::Var::Lexical.new('@a'),
    body   => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new)),
    otherwise => $blk);
ok $made.otherwise === $blk, 'new(:otherwise) is answered';
nok $made.raku.contains('otherwise'), '.raku of a constructed for leaves it out';

my $plain = RakuAST::Statement::For.new(
    source => RakuAST::Var::Lexical.new('@a'),
    body   => $blk);
is $plain.otherwise.^name, 'RakuAST::Block', 'omitted otherwise is the type object';

throws-like { $for.replace-otherwise(RakuAST::IntLiteral.new(1)) },
    X::TypeCheck::Binding::Parameter, 'a non-block is refused';

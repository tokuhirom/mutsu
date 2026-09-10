use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 4 slice 6 (ADR-0011): Blockoid and Block construction.
# This file passes under both mutsu and raku.

plan 8;

my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
    )
);

my $blockoid = RakuAST::Blockoid.new($statements);
isa-ok $blockoid, RakuAST::Blockoid, 'Blockoid.new constructs a Blockoid';
is $blockoid.statement-list, $statements,
    'Blockoid exposes its StatementList';
is $blockoid.gist, q:to/END/.chomp, 'Blockoid renders its positional StatementList';
    RakuAST::Blockoid.new(
      RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::IntLiteral.new(42)
        )
      )
    )
    END

my $block = RakuAST::Block.new(body => $blockoid);
isa-ok $block, RakuAST::Block, 'Block.new constructs a Block';
is $block.body, $blockoid, 'Block exposes its body';
is $block.gist, q:to/END/.chomp, 'Block renders its named body';
    RakuAST::Block.new(
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(42)
          )
        )
      )
    )
    END

my @block-methods = RakuAST::Block.^methods(:local)>>.name;
ok 'new' (elem) @block-methods && 'body' (elem) @block-methods,
    'Block introspection exposes its constructor and accessor';
my @blockoid-methods = RakuAST::Blockoid.^methods(:local)>>.name;
ok 'new' (elem) @blockoid-methods && 'statement-list' (elem) @blockoid-methods,
    'Blockoid introspection exposes its constructor and accessor';

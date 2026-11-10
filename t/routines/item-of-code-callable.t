use Test;

# `item` of a code object is the code object: it stays callable.

plan 5;

my $block = item({ 42 });
is $block(), 42, 'item of a block can be called';
my $sub = item(sub { 5 });
is $sub(), 5, 'item of an anonymous sub can be called';
my $pointy = item(-> $x { $x * 2 });
is $pointy(4), 8, 'item of a pointy block can be called with arguments';
is $block.WHAT.^name, 'Block', 'it is still a Block';

my $node = RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(42)))));
my $made = EVAL RakuAST::Statement::Expression.new(expression => RakuAST::Call::Name.new(
    name => RakuAST::Name.from-identifier('item'), args => RakuAST::ArgList.new($node)));
is $made(), 42, 'EVAL of item(Block node) answers the callable closure';

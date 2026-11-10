use experimental :rakuast;
use Test;

plan 6;

# `Routine.meta-object` of a routine node nothing has declared (a slang builds
# it and attaches it later) is the routine object itself, not a failure.
my $body = RakuAST::Blockoid.new(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::StrLiteral.new("smoke"))));

my $sub = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('foo'), signature => RakuAST::Signature.new, :$body);
my $sub-mo = try $sub.meta-object;
ok $sub-mo.defined, 'a Sub node has a meta-object';
isa-ok $sub-mo, Sub, 'which is a Sub';

my $method = RakuAST::Method.new(
    name => RakuAST::Name.from-identifier('foo'), signature => RakuAST::Signature.new, :$body);
my $method-mo = try $method.meta-object;
ok $method-mo.defined, 'a Method node has a meta-object';
isa-ok $method-mo, Method, 'which is a Method';

my $block = RakuAST::Block.new(:$body);
my $block-mo = try $block.meta-object;
ok $block-mo.defined, 'a Block node has a meta-object';
isa-ok $block-mo, Block, 'which is a Block';

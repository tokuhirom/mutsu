use Test;

plan 3;

# `RakuAST::Block.signature` (from `RakuAST::Code`) answers Nil for a block
# without a declared signature; a pointy block and a routine keep theirs.

my $block = Q|{ $_ = 1 }|.AST.statements[0].expression;
nok $block.signature.DEFINITE, 'a bare block has no signature';
my $pointy = Q|-> $x { 1 }|.AST.statements[0].expression;
isa-ok $pointy.signature, RakuAST::Signature, 'a pointy block has its own';
my $sub = Q|sub f($a) { }|.AST.statements[0].expression;
isa-ok $sub.signature, RakuAST::Signature, 'a routine has its own';

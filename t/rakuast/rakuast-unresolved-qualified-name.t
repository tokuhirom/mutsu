use Test;

# Rakudo 2026.09 models an unresolved but parseable qualified name as an
# argument-less Call::Name, retaining lookup failure for evaluation time.

plan 3;

my $postfix = Q[UniqueMissing7564::Thing.new].AST.statements[0].expression;
isa-ok $postfix, RakuAST::ApplyPostfix,
    'a method call on an unresolved qualified name is an ApplyPostfix';
isa-ok $postfix.operand, RakuAST::Call::Name,
    'the unresolved qualified name is an argument-less Call::Name';

is EVAL(Q[try UniqueMissing7564::Thing.new].AST), Nil,
    'an unresolved qualified lookup remains catchable after round-tripping';

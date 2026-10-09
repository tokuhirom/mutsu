use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 8;

my $call = Q[((*.flip)).assuming(42)].AST.statements[0].expression;
isa-ok $call, RakuAST::ApplyPostfix, 'assuming is a postfix application';
isa-ok $call.operand, RakuAST::Circumfix::Parentheses,
    'a visible operand circumfix records the extra parentheses';
ok $call.operand.gist.contains('RakuAST::ApplyPostfix'),
    'the inner curry is a postfix application';

is EVAL(Q[((*.flip)).assuming(42)()].AST), '24',
    'the outer method receives a finished WhateverCode';
is EVAL(Q[((* + *)).assuming(42)(3)].AST), 45,
    'a finished two-argument curry accepts a partial application';
is EVAL(Q[((*.flip)).arity].AST), 1,
    'the frozen curry retains its original arity';
dies-ok { EVAL(Q[((*)).abs].AST) },
    'a doubly parenthesized bare Whatever remains a value';
is EVAL(Q[(*.flip)(42)].AST), '24',
    'one parenthesis layer still forms a callable WhateverCode';

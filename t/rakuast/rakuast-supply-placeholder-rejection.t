use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

my $ast = Q[supply { emit 7; done }].AST;
isa-ok $ast.statements[0].expression, RakuAST::StatementPrefix::Supply,
    'a valid supply remains the measured statement prefix';
is EVAL($ast).list.join(','), '7',
    'EVAL of a valid supply prefix still emits its value';

try EVAL(Q[supply { $^c; done }].AST);
is $!.^name, 'X::Placeholder::Block',
    'a supply placeholder reaches the normal placeholder-scope diagnostic';

try EVAL(Q[start { $^c }].AST);
is $!.^name, 'X::Placeholder::Block',
    'a start placeholder reaches the normal placeholder-scope diagnostic';

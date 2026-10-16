use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 7;

my $pos = 1;
my $continue = 2;
my $ast = Q[m:p($pos):c($continue):g/a/].AST;
my $regex = $ast.statements[0].expression;
isa-ok $regex, RakuAST::QuotedRegex,
    'an immediately matched regex remains a QuotedRegex';
ok $regex.match-immediately,
    'm// retains its match-immediately field';
is $regex.adverbs.elems, 3,
    'dynamic and static match adverbs remain ordered on the node';
is $regex.adverbs[0].key, 'p',
    'the dynamic position adverb keeps its written key';
isa-ok $regex.adverbs[0].value, RakuAST::Circumfix::Parentheses,
    'the dynamic position remains an expression value';
is $regex.adverbs[1].key, 'c',
    'the dynamic continue adverb keeps its written key';
ok EVAL($ast).defined,
    'the dynamic match adverbs lower and execute';

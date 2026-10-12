use v6;
use experimental :rakuast;
use Test;

plan 3;

my $source = Q[use lib 't/lib'; use BraceSubscriptRoutes; routes];
my $expression = $source.AST.statements[2].expression;
isa-ok $expression, RakuAST::Call::Name::WithoutParentheses,
    'an imported bare routine is a Call::Name::WithoutParentheses';
is $expression.name.raku, 'RakuAST::Name.from-identifier("routes")',
    'the imported routine name is retained';

is EVAL(Q[use lib 't/lib'; use BraceSubscriptRoutes; routes].AST).{'/'}{'GET'},
    'imported-handler', 'the imported call lowers and executes';

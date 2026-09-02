use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST hyper function infix operators (ADR-0011). The internal
# HyperFuncOp maps to MetaInfix::Hyper(FunctionInfix(Var::Lexical(...))) and
# lowers back through the existing hyper-function execution path.

plan 19;

# --- read side --------------------------------------------------------------
my $strict = Q[my @a; my @b; @a >>[&infix:<+>]<< @b].AST.gist;
ok $strict.contains('RakuAST::FunctionInfix.new('),
    'a hyper function infix renders a FunctionInfix';
ok $strict.contains('RakuAST::Var::Lexical.new("\\&infix:<+>")'),
    'FunctionInfix retains the referenced code variable';
nok $strict.contains('dwim-left'), 'strict form sets no dwim-left';
nok $strict.contains('dwim-right'), 'strict form sets no dwim-right';

my $both = Q[my @a; @a <<[&infix:<+>]>> 1].AST.gist;
ok $both.contains('dwim-left') && $both.contains('=> True'), '<<[&f]>> sets dwim-left';
ok $both.contains('dwim-right => True'), '<<[&f]>> sets dwim-right';

my $right = Q[my @a; @a >>[&infix:<+>]>> 1].AST.gist;
nok $right.contains('dwim-left'), '>>[&f]>> sets no dwim-left';
ok $right.contains('dwim-right => True'), '>>[&f]>> sets dwim-right';

my $left = Q[my @a; @a <<[&infix:<+>]<< 1].AST.gist;
ok $left.contains('dwim-left => True'), '<<[&f]<< sets dwim-left';
nok $left.contains('dwim-right'), '<<[&f]<< sets no dwim-right';

# --- introspection ----------------------------------------------------------
my $function = Q[1 >>[&infix:<+>]<< 2].AST.statements[0].expression.infix.infix;
is $function.^name, 'RakuAST::FunctionInfix', 'the base node is FunctionInfix';
is $function.function.^name, 'RakuAST::Var::Lexical',
    'FunctionInfix.function is a lexical code variable';
is $function.function.name, '&infix:<+>',
    'FunctionInfix.function retains its &-sigiled name';

# --- write side -------------------------------------------------------------
is EVAL(Q[my @a = 1, 2, 3; my @b = 10, 20, 30;
    (@a >>[&infix:<+>]<< @b).join(",")].AST),
    '11,22,33', 'a strict hyper function infix round-trips through EVAL';
is EVAL(Q[my @a = 1, 2, 3; (@a >>[&infix:<+>]>> 1).join(",")].AST),
    '2,3,4', 'a right-dwim hyper function infix round-trips through EVAL';
is EVAL(Q[my @a = 1, 2, 3; my @b = 10, 20, 30;
    (@a <<[&infix:<+>]<< @b).join(",")].AST),
    '11,22,33', 'a strict-left hyper function infix round-trips through EVAL';
is EVAL(Q[my @a = 1, 2, 3; (@a <<[&infix:<+>]>> 1).join(",")].AST),
    '2,3,4', 'a both-dwim hyper function infix round-trips through EVAL';

# The model metadata remains available for a hand-built FunctionInfix node.
ok RakuAST::FunctionInfix.^can('new').elems,
    'FunctionInfix advertises its constructor';
ok RakuAST::FunctionInfix.^can('function').elems,
    'FunctionInfix advertises its function accessor';

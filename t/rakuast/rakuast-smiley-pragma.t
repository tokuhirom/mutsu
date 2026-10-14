use v6;
use experimental :rakuast;
use Test;

# `use variables :D` and the other type-smiley pragmas are a `Pragma` whose
# argument is a `ColonPair::True` of the smiley (measured on rakudo 2026.09).

plan 7;

my $pragma = Q[use variables :D].AST.statements[0];
isa-ok $pragma, RakuAST::Pragma, 'a smiley pragma is a Pragma';
is $pragma.name, 'variables', 'it keeps its name';
isa-ok $pragma.argument, RakuAST::ColonPair::True, 'its argument is a colonpair';
is $pragma.argument.key, 'D', 'of the written smiley';

is Q[use attributes :_].AST.statements[0].argument.key, '_',
    'the attributes pragma keeps its smiley too';

is EVAL(Q[use variables :D; my Int $x = 3; $x].AST), 3,
    'a :D variable with an initializer round-trips';

throws-like { EVAL Q[use variables :D; my Int $x = 3; $x = Nil].AST },
    X::TypeCheck::Assignment, 'the round-tripped pragma still applies :D';

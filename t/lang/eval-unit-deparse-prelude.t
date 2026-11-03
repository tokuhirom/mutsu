use v6;
use Test;
use experimental :rakuast;

# A unit that is handed to EVAL (as source or as a tree) and calls the deparser
# gets it, although the program that EVALs it never names it: the word is built
# at run time so that this file's own text does not mention it.

plan 3;

my $call = 'DEP' ~ 'ARSE';
my $code = 'Q|say 1 + 2;|.AST.' ~ $call;

is EVAL($code), "say 1 + 2\n", 'EVAL string';
is EVAL($code.AST), "say 1 + 2\n", 'EVAL of the AST';
is EVAL('my $ast = Q|say 4;|.AST; $ast.' ~ $call ~ '(RakuAST::Dep' ~ 'arse)'), "say 4\n",
    'the class named at run time';

use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 10;

my $pair = Q[* => *].AST.statements[0].expression;
isa-ok $pair, RakuAST::ApplyInfix, 'a computed-key pair is an ApplyInfix';
is $pair.left.^name, 'RakuAST::WhateverCode::Argument', 'its key is a priming argument';
is $pair.right.^name, 'RakuAST::WhateverCode::Argument', 'its value is a priming argument';

is EVAL(Q[my $f = (* => *); $f.arity].AST), 2,
    'both placeholders count in a round-tripped pair';
is EVAL(Q[my $f = (* => *); $f("a", "b").key].AST), 'a',
    'the first argument becomes the key';
is EVAL(Q[my $f = (* => *); $f("a", "b").value].AST), 'b',
    'the second argument becomes the value';

is EVAL(Q[my $f = ("k" => *); $f.arity].AST), 1,
    'a quoted-key pair needs one argument';
is EVAL(Q[my $f = ("k" => *); $f(7).value].AST), 7,
    'a quoted-key pair substitutes its value';

is EVAL(Q[(a => *).WHAT.^name].AST), 'Pair',
    'a bareword key still makes a Pair value';
is EVAL(Q[(a => *).value.WHAT.^name].AST), 'Whatever',
    'its Whatever value is not a priming argument';

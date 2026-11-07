use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token that calls the core `<signature>` rule gets a RakuAST::Signature
# for the parameters it matched (ADR-15762 slice 2, #13370).

plan 3;

use SignatureShimSlang;

fun add($a, $b) { $a + $b }
fun greet(Str $who, :$punct = '!') { "hi $who$punct" }

is add(2, 3), 5, 'the signature binds positional parameters';
is greet('bob'), 'hi bob!', 'a typed parameter and a named default';
is greet('bob', :punct('?')), 'hi bob?', 'named arguments bind';

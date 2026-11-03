use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 12;

# `self`, `*`, `$/`, `$!` and an attribute variable lower to themselves whatever
# the unit declares, so the executing finalization keeps them as the parser built
# them instead of converting each to RakuAST and back. The behaviour is the same
# through the program, an EVAL string and an EVAL of the tree.

class Counter {
    has $!n = 0;
    method bump { $!n++; self }
    method n { $!n }
    method me { self }
}
is Counter.new.bump.bump.n, 2, 'self and an attribute in a method';
isa-ok Counter.new.me, Counter, 'self is the invocant';

is (1, 2, 3).map(* * 2).join(','), '2,4,6', '* curries';
is (5, 6).map(*.succ).join(','), '6,7', '*.method curries';

'abc' ~~ /b/;
is ~$/, 'b', '$/ holds the match';
try { die 'oops' };
is $!.message, 'oops', '$! holds the exception';

my $unit = Q[class K { has $!v = 3; method v { $!v }; method s { self } }; K.new.v + (1, 2).map(* + 1).sum];
is EVAL($unit), 8, 'EVAL of a string';
my $unit2 = $unit.subst('K', 'K2', :g);
is EVAL($unit2.AST), 8, 'EVAL of the tree';

# A routine the unit declares does not capture the keyword.
sub self-like { 'sub' }
is self-like(), 'sub', 'a routine named like a keyword prefix is a routine';
is EVAL(Q[sub f { 7 }; f()]), 7, 'a unit routine called inside EVAL';
is EVAL(Q['xyz' ~~ /y/; ~$/]), 'y', '$/ inside EVAL';
is EVAL(Q[my $w = * + 10; $w(5)]), 15, 'a Whatever closure inside EVAL';

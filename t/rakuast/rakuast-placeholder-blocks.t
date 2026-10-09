use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# A bare block's placeholders are its signature. Through the RakuAST round trip
# a `for` body that declares several consumes that many elements per iteration,
# and a bare block statement is run once with no arguments.
plan 8;

is EVAL(Q[my @seen; for 1..8 { @seen.push: $^a + $^b }; @seen.join(",")].AST), '3,7,11,15',
    'two placeholders sum consecutive pairs';
is EVAL(Q[my @seen; for 1..4 { @seen.push: "$^a $^b" }; @seen.join("|")].AST), '1 2|3 4',
    'two placeholders consume two per iteration';
is EVAL(Q[my @seen; for 1..3 { @seen.push: $^a * 10 }; @seen.join(",")].AST), '10,20,30',
    'one placeholder binds one element';
is EVAL(Q[my @seen; for 1..3 { @seen.push: $_ }; @seen.join(",")].AST), '1,2,3',
    'a body without placeholders keeps the topic';

dies-ok { EVAL Q[{ my $x = $^a }].AST }, 'a bare block statement with a placeholder is called with no arguments';
lives-ok { EVAL Q[{ my $x = 1 }].AST }, 'one without lives';
is EVAL(Q[my $c = { $^a + 1 }; $c(4)].AST), 5, 'a block bound to a variable stays a closure';
is EVAL(Q[{ $^a + $^b }(1, 2)].AST), 3, 'a called block receives its arguments';

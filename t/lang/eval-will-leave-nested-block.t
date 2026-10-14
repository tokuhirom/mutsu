use v6;
use Test;

plan 3;

# `will leave` on a variable in a block nested inside an EVAL unit runs when
# the block exits (#13330), like a plain LEAVE phaser.
is EVAL(q[my $log = ""; { my $v will leave { $log ~= "left" }; $log ~= "in;" }; $log]),
    "in;left", 'will leave in a nested block of an EVAL string';
is EVAL(q[my $log = ""; { LEAVE { $log ~= "left" }; $log ~= "in;" }; $log]),
    "in;left", 'plain LEAVE in the same position';
my $log = "";
{ my $v will leave { $log ~= "left" }; $log ~= "in;" }
is $log, "in;left", 'will leave outside EVAL';

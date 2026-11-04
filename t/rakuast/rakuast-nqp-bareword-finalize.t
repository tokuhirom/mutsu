use Test;

plan 3;

# An `nqp::` op written as a bareword term (mutsu's own batteries do this; rakudo
# needs parentheses) is resolved by the classifier and lowered where the parser
# built it (ADR-15760 slice 2), as an argument-less op call.

use nqp;
my $now = nqp::time;
ok $now > 0, 'a bareword nqp op is an argument-less call';
ok nqp::const::CCLASS_WHITESPACE > 0, 'a bareword nqp constant';
is nqp::elems(nqp::list(1, 2)), 2, 'ops with arguments are unchanged';

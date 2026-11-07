use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's actions role may override a core action (`INFIX-EXPR`): it calls
# `callsame` for the node the core action built and attaches a rewritten one
# (ADR-15762 slice 3, #13369). Here `+` is rewritten into `-`.

plan 9;

use InfixRewriteSlang;

is 10 + 3, 7, 'an infix application the action rewrites';
is 10 - 3, 7, 'an application it leaves alone keeps the core node';
is 2 * 3 + 1, 5, 'the rewrite applies to a nested application (6 - 1)';
my $x = 5;
is $x + 1, 4, 'and to operands that are variables';
is 1 + 2 + 3, -4, 'and to a chain of applications, bottom-up (left to right)';
is (10 + 3, 2 + 2).join(','), '7,0', 'in list elements';
is "a" ~ "b", 'ab', 'other operators are untouched';
is !True, True, 'a PREFIX-EXPR override drops the prefix';
is (!5) + 1, 4, 'and its result is an operand of an infix application';

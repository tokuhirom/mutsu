use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's actions role may override the core `POSTFIX-EXPR` action
# (ADR-15762 slice 3, #13369). Here a postfix `++` becomes a call of `bump`.

plan 4;

use PostfixRewriteSlang;

my $x = 5;
is $x++, 105, 'a postfix application the action rewrites';
is $x, 5, 'and the operand itself is not incremented';
my @a = 1, 2;
is @a.elems, 2, 'other postfix forms are untouched';
is (3, 4).map({ $_++ }).join(','), '103,104', 'in a block';

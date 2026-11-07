use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# `$*LANG.refine_slang('MAIN', $grammar-role, $actions-role)` with an actions
# role that overrides the core `numish` action (ADR-15762, #15804): every numeric
# literal after the `use` goes through it. Here an Int literal is scaled by 10, so
# the expected values below are strings (a string is not numish).

plan 5;

use NumishSlang;

is 7, '70', 'an Int literal is rewritten by the action';
is 1 + 2, '30', 'each operand of an expression is';
is 2.5, '2.5', 'a literal the action declines keeps the core node';
is "7", '7', 'a string is not numish';
is (3, 4).join(','), '30,40', 'list elements are';

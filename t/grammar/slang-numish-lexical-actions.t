use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# `$*LANG.refine_slang` reads the members of a lexical `my role` actions role, not only
# of an `our`/package role (#17008).

plan 3;

use NumishMyRoleSlang;

is 7, '70', 'a lexical `my role` actions role is read';
is 1 + 2, '30', 'each operand goes through it';
is 2.5, '2.5', 'a literal the action declines keeps the core node';

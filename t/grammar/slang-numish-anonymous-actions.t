use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# `$*LANG.refine_slang` reads the members of an anonymous actions role that declares the action, not only
# of an `our`/package role (#17008).

plan 3;

use NumishAnonRoleSlang;

is 7, '70', 'an anonymous actions role that declares the action is read';
is 1 + 2, '30', 'each operand goes through it';
is 2.5, '2.5', 'a literal the action declines keeps the core node';

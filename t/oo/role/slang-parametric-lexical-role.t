use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# `$*LANG.refine_slang('MAIN', role { }, Numish[%opts])` with a lexical parametric
# actions role: the members of the declaration the instance came from are read
# (overload::constant). The `use` argument reaches `sub EXPORT`, so the factor is
# 10 and every Int literal after the `use` is scaled by it; the expected values
# are strings because a string literal is not numish.

plan 3;

use ParamRoleSlang 10;

is 4, '40', 'an Int literal goes through the parametric role\'s numish action';
is 1 + 2, '30', 'each operand does';
is 2.5, '2.5', 'a literal the action declines keeps the core node';

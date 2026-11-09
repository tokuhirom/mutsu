use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang production may install a constant for the unit that imports it through
# `$*R.current-scope.merge-generated-lexical-declaration` (#13370): the name is
# a lexical of the importing unit, bound to the value the production computed.

plan 3;

use GeneratedConstantSlang;

defconst Answer 42;
is Answer, 42, 'the installed constant is visible after the statement';
is Answer + 1, 43, 'it is usable in an expression';

defconst Other 7;
is Answer + Other, 49, 'a second production installs a second constant';

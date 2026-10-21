use Test;
use lib 't/lib';

# A package-less module's `my constant` is lexical to its compunit: the
# importing program's EVAL does not see it, while the module's own routines
# still read it (#14330). A bare `constant` stays visible, as in Rakudo.
plan 5;

use Issue14330MyConstant;

is huff(), 10, "the module's routine reads its my constants";
is (try EVAL 'HUFF') // 'not visible', 'not visible', 'a sigilless my constant is not visible';
is (try EVAL '@TABLE') // 'not visible', 'not visible', 'a my constant array is not visible';
is (try EVAL 'OPEN') // 'not visible', 4, 'a bare constant is visible';
is (try EVAL '$count') // 'not visible', 'not visible', 'a my variable is not visible';

use lib 't/lib';
use Test;

# A `use X:if(COND)` inside a module (under the `if` pragma module) binds the
# routines it imports lexically like an unconditional `use`, so a bare-word
# call of an imported routine in the module's own sub resolves to the routine,
# not the string (#16583, Universal::Errno).

use CondUseErrno;

plan 1;

is chk(), 'errno', 'bare imported routine under a conditional use resolves in a module sub';

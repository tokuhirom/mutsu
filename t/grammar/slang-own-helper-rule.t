use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's grammar role may declare a rule rakudo's own grammar lacks, whether
# or not another rule of the role calls it (ADR-16888): it is the slang's own
# helper, called from a method or from a production the parser hooks. Only an
# override of a core rule is refused.

plan 2;

use OwnHelperRuleSlang;

is 1 + 2, 3, 'a slang with unreferenced helper rules activates';
is "a" ~ "b", 'ab', 'and the rest of the unit parses as before';

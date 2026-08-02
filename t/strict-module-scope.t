use Test;
use lib 't/lib';

plan 3;

no strict;
use StrictPragmaFixture;

is fixture-value(), 42, 'module with use strict loads normally';
$module_strict_leak = 5;
is $module_strict_leak, 5, 'module use strict does not leak into a lax caller';

use strict;
use StrictPragmaFixture;
throws-like '$caller_is_strict = 6;', X::Undeclared,
    'loading an existing module preserves a strict caller';

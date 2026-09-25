use v6;
use lib 't/lib';
use Test;

# Regression (#9410, Scalar::Util's `readonly`): a raw `\a` parameter of a sub
# imported through a custom `sub EXPORT` must bind the caller's container, as
# the same sub does when imported by a plain `is export` or called through a
# code variable. The `&name` override path dispatched without the call site's
# argument sources, so `a.VAR` saw a bare value.

use ExportHookRawParam <ro rv bump>;

plan 5;

my $a = 42;
is ro($a), False, 'a variable is a container (nqp::iscont)';
is rv($a), 'Scalar', 'a.VAR of a variable is its Scalar';
is ro(42), True, 'a literal is not a container';
bump($a);
is $a, 43, 'writing through the raw parameter reaches the caller';
dies-ok { bump(42) }, 'writing through a raw parameter bound to a literal dies';

use v6;
use lib 't/lib';
use Test;

plan 1;

# The no-smiley sibling of role-param-qualified-type-unrelated-module.t (see
# that file for the full explanation of #8023). Kept in its own process,
# like that file, for the reason explained there (#8084).

use RoleUnrelatedModuleUserNoSmiley;

class Consumer does RoleUnrelatedModuleUserNoSmiley {}

is Consumer.new.self-test(), "qualified-no-smiley-ok",
    'the same holds with no definiteness smiley';

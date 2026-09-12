use v6;
use lib 't/lib';
use Test;

plan 1;

# #8084: the same body-imported qualified type must remain resolvable when two
# separate roles check it in one process.

use RoleUnrelatedModuleUserReuseA;
use RoleUnrelatedModuleUserReuseB;

class FirstRoleConsumer does RoleUnrelatedModuleUserReuseA {}
class SecondRoleConsumer does RoleUnrelatedModuleUserReuseB {}

is FirstRoleConsumer.new.self-test() ~ SecondRoleConsumer.new.self-test(), 'ab',
    'a body-imported qualified type can be reused across two roles';

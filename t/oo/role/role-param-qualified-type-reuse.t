use v6;
use lib 't/lib';
use Test;

plan 1;

# #8084: a qualified type imported by a role body was put into a caller cell
# when passed to an implicitly raw sigilless parameter. The first method call
# worked, but the second lookup saw the ContainerRef instead of the imported
# type object and rejected the same argument.

use RoleUnrelatedModuleUserReuse;

class SameRoleConsumer does RoleUnrelatedModuleUserReuse {}

is SameRoleConsumer.new.self-test(), 'm1m2',
    'a body-imported qualified type can be reused by two methods in one role';

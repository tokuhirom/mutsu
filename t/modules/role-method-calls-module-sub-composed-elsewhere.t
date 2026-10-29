use v6;
use lib 't/fixtures/role-method-module-sub/lib';
use Test;
use RoleSubMod;

plan 2;

# A role method declared in a module calls a sub of that module; the role is
# composed into a class declared in another file, so the method runs under the
# composing class's package.
class Host does Greets { }

is Host.new.greet, "h!", 'role method reaches its module sub from a foreign class';

role Wrapper does Greets { }
class Host2 does Wrapper { }
is Host2.new.greet, "h!", 'also through a role composed from the role';

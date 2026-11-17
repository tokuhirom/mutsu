use Test;
use lib 't/lib-role-proto-imports';
use RoleProtoImports;

# A role's `proto method` body sees the routines its module imports, also
# when a class in another compunit composes it (LibTmux::Model).

plan 3;

is K1.new.go(5), 'plain/admitted 1 int 5', 'proto body of a plain role calls imported subs';
is K2.new.go(7), 'admitted 1 k2 typed 7', 'proto body of a parametric role calls imported subs';

class K3 does Plain { }
is K3.new.go(9), 'plain/admitted 1 int 9', 'a class here composing the role';

use v6;
use lib 't/lib';
use Test;

plan 2;

# #8083: a role method whose parameter names a GENUINELY mistyped type must
# not be silently accepted just because the role body `use`s a module that
# might have supplied it. The role body's `use` is BEGIN-time, so it has run
# by the time the method is declared, and the typo is reported when the role
# is declared -- i.e. when its module loads, as rakudo reports it ("Invalid
# typename 'TotallyBogusTypeName:U' in parameter declaration").

try EVAL q[use RolePendingTypo::R];
ok $!.defined, 'loading the role dies';
ok $!.message.contains(q[Invalid typename 'TotallyBogusTypeName:U' in parameter declaration]),
    'with the invalid-typename error for the mistyped role-method param type';

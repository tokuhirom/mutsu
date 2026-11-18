use lib 't/lib';
use Test;

# An exported `sub` in a role body keeps reading the body's `my constant`
# after the role is punned from another module's code: the pun must not
# declare the sub again in that module's package, where it shadowed the
# module's import and found no `%D` (#18106, CSS::Properties).

plan 3;

use RolePunExportedSubUser;
is RolePunExportedSubUser.dd, 0.75, 'the imported sub reads the constant';
is RolePunExportedSubUser.mk.type, 'px', 'the module puns the role';
is RolePunExportedSubUser.dd, 0.75, '... and the imported sub still reads it';

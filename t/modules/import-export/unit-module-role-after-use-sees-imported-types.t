use Test;
use lib 't/lib-unit-module-role-shell';

# A `unit module` declarator runs no code, so a role declared after the
# module's `use NativeCall` is registered after that import: the traits of the
# role body's subs see the imported `size_t`, not an unresolved stub
# (#18092, NativeHelpers::Blob's `role LinearArray` with a `sub calloc(size_t,
# size_t --> Pointer) is native`, which warned "Not an accepted NativeCall type"
# once another module had loaded NativeCall).

plan 3;

use ShellStore;
use ShellFirst;
use ShellRole;
use ShellRoleLate;

is R.new.z, 1, 'the role composes';
is RL.new.z, 2, 'a role after run-time code and a versioned unit declarator composes';
ok @ShellStore::SEEN && (all(@ShellStore::SEEN) eq 'NativeCall::Types::size_t'),
    'every trait run, in both modules, sees the imported type';

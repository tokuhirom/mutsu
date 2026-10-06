use Test;

# From the Injector distribution: it enumerates the modules `use` loaded via
# `$*REPO.repo-chain.flatmap(*.loaded)`.
plan 2;

use lib 't/lib';
use AddParentScopedBase;

my @names = $*REPO.repo-chain.flatmap(*.loaded).map(*.Str);
ok 'AddParentScopedBase' (elem) @names, 'a used module is listed under its repository';
ok !('No::Such::Module' (elem) @names), 'unloaded modules are not';

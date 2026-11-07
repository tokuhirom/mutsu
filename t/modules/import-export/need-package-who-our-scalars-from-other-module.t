# `Pkg.WHO` lists a `need`ed package's `our` variables when taken inside another module's
# routine (#17054): the stash belongs to the package object, not to the unit reading it.
use lib 't/lib';
use Test;
use WhoStashReader;
need WhoStashOwner;
plan 4;
is keys-of(WhoStashOwner), '$v1,$v2,@list,EXPORT', 'the stash read inside another module has the unexported our variables';
is WhoStashOwner.WHO.keys.sort.join(','), '$v1,$v2,@list,EXPORT', 'and the unit that needed the package sees the same keys';
is value-of(WhoStashOwner, '$v1'), 42, 'an our scalar read through the stash from another module';
is-deeply value-of(WhoStashOwner, '@list').List, (1, 2), 'an our array read through the stash from another module';

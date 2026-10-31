use Test;
use lib $*PROGRAM.parent(2).add('fixtures/resources-undeclared-key/lib');
use ResUndeclared;
plan 9;

# Inline::Python3 guards on `%?RESOURCES<libraries/python3_helper>` for a file
# its META6.json does not list: rakudo answers a truthy, path-less
# Distribution::Resource for every key.
my $none = undeclared();
ok $none.defined, 'an undeclared key answers a defined object';
ok $none.Bool, 'which is truthy';
is $none.IO.raku, 'Empty', 'with no path to read';
is ~$none, '', 'and an empty string form';

my $lib = declared-library();
ok $lib.Bool, 'a declared key is truthy';
is $lib.IO.basename, 'plain', 'a libraries/ resource shipped under exactly its declared name is that file';
is declared-data().IO.basename, 'data.txt', 'a declared plain resource keeps its path';
is declared-data().IO.slurp.chomp, 'hello', 'and can be read';
ok (undeclared() ?? True !! False), 'the guard idiom `return %?RESOURCES<k> if %?RESOURCES<k>` takes the branch';

use Test;
use lib $*PROGRAM.parent(3).add('fixtures/resources-undeclared-key/lib');
use ResUndeclared;
plan 10;

# A `%?RESOURCES` entry is a Distribution::Resource, not an IO::Path (#15818).
my $d = declared-data();
is $d.^name, 'Distribution::Resource', '.^name';
is $d.WHAT.raku, 'Distribution::Resource', '.WHAT';
ok $d ~~ Distribution::Resource, 'smartmatches Distribution::Resource';
nok $d ~~ IO::Path, 'is not an IO::Path';
is $d.slurp.chomp, 'hello', 'slurp reads the file';
ok $d.IO ~~ IO::Path, '.IO is an IO::Path';
is $d.IO.^name, 'IO::Path', '.IO class name';
is $d.IO.basename, 'data.txt', '.IO keeps the path';
is undeclared().^name, 'Distribution::Resource', 'an undeclared key is a Distribution::Resource too';
nok undeclared() ~~ IO::Path, 'and not an IO::Path';

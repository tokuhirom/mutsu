use v6;
use lib $*PROGRAM.parent(3).add('lib-exported-our-arrays');
use Test;
use ExportedOurArrays;

# Uppercase-initial `our` aggregates a module exports are ordinary variables,
# not type names: closures in the module and in the importer capture them only
# when they name them, and still see their live contents. Classing them as
# system names put every exported array into every closure capture (#19324).

plan 9;

is square-sum(4), 0 + 1 + 4 + 9 + 16 - 0, 'a module closure reads the module array';
is (^3).map({ @Squares[$_] }).list, (1, 4, 9), 'an importer closure reads an exported array';
is @Fib[^8], (0, 1, 1, 2, 3, 5, 8, 13), 'an exported lazy sequence reifies';
is fib-pairs(5).map(*.value).list, (0, 1, 1, 2, 3), 'a module gather reads and writes module state';
is %Memo.elems, 5, 'the gather wrote the exported hash';
is qualified-first(), 1, 'a closure reads the package-qualified spelling';

my $c = { %Memo{'extra'} = 42 };
$c();
is %ExportedOurArrays::Memo<extra>, 42, 'an importer closure writes through to the package variable';

my @Local = 1, 2;
my &count = { @Local.elems };
@Local.push(3);
is count(), 3, 'a closure sees a later push to an uppercase local array';

my %Seen;
my @words = <a b a c b a>;
.&{ %Seen{$_}++ } for @words;
is %Seen<a>, 3, 'a block call writes an uppercase local hash';

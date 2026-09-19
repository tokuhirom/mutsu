use v6.d;
use Test;

plan 1;

my @nested = [<a b>, <c d>];
my $gathered = gather @nested.deepmap: *.take;

is $gathered.map({ [$_,] }).Array.raku,
    '[["a"], ["b"], ["c"], ["d"]]',
    'deepmap takes all leaves when its gather is consumed lazily';

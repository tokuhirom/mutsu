use Test;

plan 5;

# An array-backed iterator answers each pull-one without copying its items:
# a pull loop over n elements is O(n), not O(n^2) (#16475). 60000 pulls took
# minutes when every call copied the 60000-element array.
my $n = 60000;
my $it = (^$n).List.iterator;
my $sum = 0;
my $count = 0;
loop {
    my \v = $it.pull-one;
    last if v =:= IterationEnd;
    $sum += v;
    $count++;
}
is $count, $n, 'every element was pulled once';
is $sum, ($n * ($n - 1)) div 2, 'the pulled elements sum correctly';

my $it2 = (1, 2, 3, 4, 5).iterator;
is $it2.pull-one, 1, 'first pull';
is $it2.skip-one, 1, 'skip-one keeps the cursor consistent';
is-deeply $it2.pull-one, 3, 'the pull after a skip continues in place';

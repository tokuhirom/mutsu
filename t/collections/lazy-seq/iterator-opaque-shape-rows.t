use Test;

# A built-in iterator answers the Iterator protocol through opaque shapes
# decoded from its attributes: PredictiveIterator rows only when it knows how
# many elements it has left.
plan 14;

my $it = (1, 2, 3).iterator;
is $it.^can('count-only').elems, 1, 'an array-backed iterator is a PredictiveIterator';
is (1..*).iterator.^can('count-only').elems, 0, 'an unending range iterator is not';
is (1, 2, 3).map(* * 2).iterator.^can('count-only').elems, 0, 'a map stream is not';
is $it.^can('pull-one').elems, 1, 'every iterator has pull-one';

is $it.pull-one, 1, 'pull-one';
is $it.pull-one, 2, 'pull-one advances';
is $it.count-only, 1, 'count-only counts what is left';
is $it.bool-only, True, 'bool-only';
$it.skip-one;
is $it.pull-one, IterationEnd, 'skip-one then the end';

my $s = (1, 2, 2, 3).squish.iterator;
is $s.pull-one, 1, 'a squish scan pulls';
is $s.pull-one, 2, 'and squishes';

my @held = (4, 5, 6).iterator;
is @held[0].pull-one, 4, 'an iterator held in an array element';
is @held[0].pull-one, 5, 'advances in place';

my $all = (7, 8).iterator;
my @got;
$all.push-all(@got);
is @got.join(','), '7,8', 'push-all';

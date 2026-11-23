use Test;

# #15962: `.flat` of a long List is a deferred pull, so creating it is O(1)
# and an `Iterable` instance among the elements is asked for its `iterator`
# only when a pull reaches it, as rakudo's lazy `flat` does. The result is
# the same Seq the eager flattener built.

plan 14;

my @log;
class C does Iterable {
    method iterator { @log.push('iter'); (7, 8).iterator }
}

my $l = (|(1..100), C.new, (200, (300, 301)), $[400, 401], $(500, 501));
my $f = $l.flat;
is-deeply @log, [], 'creating the flat runs no iterator';
is-deeply $f.head(3).List, (1, 2, 3), 'a prefix pull stops before the instance';
is-deeply @log, [], 'so the instance was not asked yet';

my @all = $l.flat;
is @all.elems, 107, 'nested Lists flatten, an itemized Array and List stay single';
is-deeply @all[100..104].List, (7, 8, 200, 300, 301), 'the instance yields its own elements';
is-deeply @all[105], [400, 401], 'the itemized Array stays one item';
is-deeply @all[106], (500, 501), 'the itemized List stays one item';
is-deeply @log, ['iter'], 'the instance was asked once';

my $big = (1..1_000_000).list.eager;
is-deeply $big.flat.head(2).List, (1, 2), 'head of a large List flat';
is $big.flat.elems, 1_000_000, 'the whole large List flattens';

is-deeply (1..70).list.flat.map(* * 2).head(3).List, (2, 4, 6),
    'a map chained onto the flat';
my @seen;
for (1..70).list.flat { last if $_ > 2; @seen.push($_) }
is-deeply @seen, [1, 2], 'a for loop streams it';

is-deeply (gather { take 1; take (2, 3) }, |(1..70)).flat.head(4).List, (1, 2, 3, 1),
    'a nested gather Seq is read by the pull';

my $once = (1..70).list.flat;
$once.List;
throws-like { $once.List }, X::Seq::Consumed, 'the flat Seq is single-use';

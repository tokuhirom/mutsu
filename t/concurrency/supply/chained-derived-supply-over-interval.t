use Test;

plan 9;

# Derived Supplies over `Supply.interval` deliver their values (#17106), and a
# stacked stage does not lose the tick that is emitted before its consumer has
# finished attaching (#17167).
is-deeply Supply.interval(0.01).head(3).list, (0, 1, 2), 'head(N).list';
is-deeply Supply.interval(0.01).head(3).Seq.list, (0, 1, 2), 'head(N).Seq.list';
is Supply.interval(0.01).head(2).Seq.elems, 2, 'head(N).Seq.elems';
is-deeply Supply.interval(0.01).head(3).Array, [0, 1, 2], 'head(N).Array';
is-deeply Supply.from-list(1, 2, 3).head(2).Seq.list, (1, 2), 'a cold Supply head(N).Seq still works';

is-deeply Supply.interval(0.01).head(3).map(* + 10).list, (10, 11, 12), 'head(N).map.list';
is-deeply Supply.interval(0.01).grep(* %% 2).head(2).list, (0, 2), 'grep.head(N).list';
my @a;
my $p = Promise.new;
Supply.interval(0.01).map(* * 2).head(2).tap({ @a.push($_) }, done => { $p.keep });
await $p;
is-deeply @a, [0, 2], 'map.head(N).tap sees every value before done';
my @b;
my $q = Promise.new;
Supply.interval(0.01).head(3).map(* + 10).tap({ @b.push($_) }, done => { $q.keep });
await $q;
is-deeply @b, [10, 11, 12], 'head(N).map.tap sees every value before done';

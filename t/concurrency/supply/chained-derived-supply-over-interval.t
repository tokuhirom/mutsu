use Test;

plan 5;

# Derived Supplies over `Supply.interval` deliver their values (#17106). Only
# the shapes that do not depend on the first tick racing the consumer's attach
# are pinned: `Supply.interval(...).head(N)` alone, drained by `.list`.
is-deeply Supply.interval(0.01).head(3).list, (0, 1, 2), 'head(N).list';
is-deeply Supply.interval(0.01).head(3).Seq.list, (0, 1, 2), 'head(N).Seq.list';
is Supply.interval(0.01).head(2).Seq.elems, 2, 'head(N).Seq.elems';
is-deeply Supply.interval(0.01).head(3).Array, [0, 1, 2], 'head(N).Array';
is-deeply Supply.from-list(1, 2, 3).head(2).Seq.list, (1, 2), 'a cold Supply head(N).Seq still works';

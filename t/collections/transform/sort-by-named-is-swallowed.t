use Test;

# No `sort` candidate declares a `:by` named parameter: the method's and the
# sub's `*%_` swallow it, and the default order applies (#17844). `min`/`max`/
# `minmax` do declare `:&by` and keep it.

plan 7;

is-deeply (3, 1, 2).sort(:by(-*)).list, (1, 2, 3), '.sort(:by(...)) ignores :by';
is-deeply [3, 1, 2].sort(:by({ $^b <=> $^a })).list, (1, 2, 3), 'a comparator in :by is ignored too';
is-deeply sort(:by(-*), 3, 1, 2).list, (1, 2, 3), 'sort(:by(...), ...) ignores :by';
is-deeply (3, 1, 2).sort(-*).list, (3, 2, 1), 'a positional mapper still sorts';
is-deeply (3, 1, 2).sort(:k).list, (1, 2, 0), ':k still selects indices';
is-deeply sort(:k, 3, 1, 2).list, (1, 2, 0), 'the sub form keeps :k';
is (1, 2, 3).max(:by(-*)), 1, 'max keeps its declared :by';

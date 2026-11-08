use Test;
plan 5;

# A Supply derived from a timed `Supply.interval` (`.head(N)`) has no channel or
# values of its own; `.list` and `.Array` tap it and collect what it emits.
is-deeply Supply.interval(0.001).head(2).list, (0, 1), ".head(2).list";
is Supply.interval(0.001).head(2).list.elems, 2, ".list.elems";
is-deeply Supply.interval(0.001).head(3).Array, [0, 1, 2], ".head(3).Array";
is-deeply Supply.from-list(1, 2, 3).head(2).list, (1, 2), "a static supply is unchanged";
is-deeply Supply.from-list(1, 2, 3).map(* * 2).list, (2, 4, 6), "a static map is unchanged";

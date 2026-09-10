use Test;

# Numeric aggregation/coercion methods on a Seq (the result of `.map`/`.grep`,
# etc.) must compute over its elements, like an Array. Previously `.sum`/`.Int`
# threw "No such method ... for invocant of type 'Seq'" and `.min`/`.max`
# returned the Seq itself instead of the extremum.

plan 12;

is (1, 2, 3).map(* ** 2).sum, 14, '.map(...).sum';
is (1 .. 5).grep(* > 2).sum, 12, '.grep(...).sum';
is (1, 2, 3).map(* + 0).min, 1, '.map(...).min';
is (1, 2, 3).map(* + 0).max, 3, '.map(...).max';
is (1, 2, 3).map(* + 0).Int, 3, '.map(...).Int is the element count';

# Explicit Seq.
is (5, 3, 8, 1).Seq.min, 1, 'Seq.min';
is (5, 3, 8, 1).Seq.max, 8, 'Seq.max';
is (5, 3, 8, 1).Seq.sum, 17, 'Seq.sum';
is <c a b>.Seq.min, 'a', 'Seq.min on strings';

# Empty Seq.
is ().Seq.sum, 0, 'empty Seq.sum is 0';

# A chained reduction still works.
is (1 .. 10).grep(* %% 2).map(* * 10).sum, 300, 'grep+map+sum chain';

# Array behavior is unchanged (regression guard).
is (3, 1, 2).min, 1, 'Array.min still works';

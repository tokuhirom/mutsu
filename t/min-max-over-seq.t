use Test;

# min()/max() sub form must fold a single iterable argument. A lone Seq was
# previously returned unfolded (`min((5,3,8).Seq)` == the Seq itself) because
# the single-arg native fast path only special-cased Hash/Array.

plan 20;

# --- single lazy Seq (the reported bug) ---
is min((5, 3, 8).Seq), 3, 'min over a single Seq folds';
is max((5, 3, 8).Seq), 8, 'max over a single Seq folds';
is min((3, 1, 2).Seq), 1, 'min over another Seq';

# --- other single-collection shapes ---
is min((5, 3, 8)),        3, 'min over a List';
is max((5, 3, 8)),        8, 'max over a List';
is min((5, 3, 8).Slip),   3, 'min over a Slip';
is min([5, 3, 8]),        3, 'min over an Array';
is max([5, 3, 8]),        8, 'max over an Array';
is min(3, 1, 2),          1, 'min over separate positional args';

# --- non-Int arrays must order numerically, not stringwise ---
is min([2.0, 10]), 2, 'min([2.0, 10]) orders numerically (not "10" < "2.0")';
is max([2.0, 10]), 10, 'max([2.0, 10]) orders numerically';
is min([10, 9, 3]), 3, 'min([10, 9, 3]) is numeric, not stringwise';

# --- strings ---
is min("b", "a", "c"), 'a', 'min over string args';
is min(<b a c>),       'a', 'min over a word list';

# --- ranges ---
is min(1..5), 1, 'min over a Range';
is max(1..5), 5, 'max over a Range';

# --- hashes: ordered by key ---
is min({ a => 3, b => 1 }).key, 'a', 'min over a Hash picks the lowest key';
is max({ a => 3, b => 1 }).key, 'b', 'max over a Hash picks the highest key';

# --- single scalar ---
is min(5), 5, 'min of a lone scalar is itself';

# --- method form still works ---
is (5, 3, 8).Seq.min, 3, 'the .min method form is unaffected';

done-testing;

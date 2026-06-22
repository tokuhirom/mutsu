use Test;

# `.pick(N)` / `.pick(*)` on a Range returns a Seq (like on a List/Array), not
# a List. The integer-range fast path (`range_pick_n_fast`) used to return a
# `Value::array`, diverging from `<a b c>.pick(*)` (already a Seq).

plan 10;

is (1..10).pick(3).WHAT.^name, 'Seq', '(1..10).pick(3) is a Seq';
is (1..10).pick(*).WHAT.^name, 'Seq', '(1..10).pick(*) is a Seq';
is (1..10).pick(Inf).WHAT.^name, 'Seq', '(1..10).pick(Inf) is a Seq';
is (5..1).pick(*).WHAT.^name, 'Seq', 'empty range .pick(*) is a Seq';
is (1..10).pick(0).WHAT.^name, 'Seq', '.pick(0) is a Seq';

# Behaviour preserved
is (1..10).pick(3).elems, 3, '.pick(3) returns 3 elements';
is-deeply (1..5).pick(*).sort.List, (1, 2, 3, 4, 5), '.pick(*) is a full shuffle';
is (1..5).pick(*).elems, 5, '.pick(*) returns all elements';
is (5..1).pick(*).elems, 0, 'empty range .pick(*) is empty';

# Large range fast path stays lazy/correct
is (1..1000000000000).pick(2).elems, 2, '.pick(2) on a huge range';

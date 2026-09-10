use Test;

# `∈` / `(elem)` (and its reverse `∋` / `(cont)`) coerce the RHS to a Set and
# test membership by `===` (object identity / `.WHICH`), NOT by stringification.
# Regression: mutsu compared list elements by `.to_string_value()`, so an Int
# `42` wrongly tested True against a list of IntStr allomorphs `< 42 >`, and a
# Str `"1"` wrongly tested True against `(1,)`.

plan 14;

# Type-distinguished scalars are NOT identical
nok 1 ∈ (1.0, 2), 'Int is not (elem) a list containing the Rat 1.0';
ok  1 ∈ (1, 2),   'Int 1 (elem) (1, 2)';
nok "1" ∈ (1, 2), 'Str "1" is not (elem) (1, 2)';
nok 1 ∈ ("1", 2), 'Int 1 is not (elem) ("1", 2)';
ok  1.0 ∈ (1.0, 2), 'Rat 1.0 (elem) (1.0, 2)';

# Allomorph identity: a plain number is not identical to the allomorph word
nok 42 ∈ < 42 43 >,   'Int 42 is not (elem) the IntStr word list < 42 43 >';
ok  <42> ∈ < 42 43 >, 'IntStr <42> (elem) < 42 43 >';
nok 42+0i ∈ < 42+0i 42 42e10 42.1 >, 'Complex is not (elem) allomorph words';
nok 42.1  ∈ < 42+0i 42 42e10 42.1 >, 'Rat is not (elem) allomorph words';

# (cont) is just the reverse and follows the same identity rule
ok  < 42 43 > ∋ <42>, 'word list (cont) IntStr <42>';
nok < 42 43 > ∋ 42,   'word list does not (cont) plain Int 42';

# Set / Bag membership still works for identical elements
ok  3 ∈ set(1, 2, 3), 'Int (elem) set of Ints';
nok 5 ∈ set(1, 2, 3), 'absent Int not (elem) set';
ok  2 ∈ bag(1, 2, 2), 'Int (elem) bag';

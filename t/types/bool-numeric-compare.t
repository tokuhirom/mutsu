use v6;
use Test;

# A Bool numifies (False -> 0, True -> 1) in ordered comparison (cmp / <=> /
# before / after / min / max / sort), so `0 cmp False` is Same, matching Rakudo.
# Regression: mutsu fell back to string comparison ("0" vs "False"), mis-ordering
# a Bool against a number and breaking min/max tie-breaking.

plan 21;

# cmp / <=> are Same for equal numeric-vs-Bool values.
is (0 cmp False), Same, '0 cmp False is Same';
is (False cmp 0), Same, 'False cmp 0 is Same';
is (0 <=> False), Same, '0 <=> False is Same';
is (1 cmp True),  Same, '1 cmp True is Same';
is (True cmp False), More, 'True cmp False is More';
is (False cmp True), Less, 'False cmp True is Less';
is (True <=> False), More, 'True <=> False is More';

# before / after.
ok !(False before 0), 'False before 0 is False (equal)';
ok !(0 after False),  '0 after False is False (equal)';
ok (False before True), 'False before True';
ok (True after False),  'True after False';

# min / max keep the FIRST argument on a numeric tie (0 == False).
is (min 0, False), 0,     'min 0, False keeps first (0)';
is (min False, 0), False, 'min False, 0 keeps first (False)';
is (max 0, False), 0,     'max 0, False keeps first (0)';
is (max False, 0), False, 'max False, 0 keeps first (False)';
is (0 min False), 0,      'infix 0 min False';
is (False min 0), False,  'infix False min 0';

# .min / .max methods over a list.
is (0, False).min, 0,      '(0, False).min';
is (False, 0).min, False,  '(False, 0).min';

# sort orders Bools by their numeric value, interleaved with numbers.
is-deeply (3, True, 2, False).sort.List, (False, True, 2, 3),
    'sort interleaves Bool by numeric value';
is-deeply (True, False).min, False, '(True, False).min';

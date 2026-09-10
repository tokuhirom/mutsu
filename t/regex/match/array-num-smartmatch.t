use Test;

plan 10;

# A list/array smart-matched against a number compares numerically (`@a == $n`),
# and a list's numeric value is its element count.

ok  [1, 2, 3]    ~~ 3,  '3-elem array ~~ 3 (elem count)';
nok [1, 2, 3]    ~~ 2,  '3-elem array !~~ 2';
ok  [1, 2, 3, 4] ~~ 4,  '4-elem array ~~ 4';
ok  (1, 2, 3)    ~~ 3,  '3-elem list ~~ 3';
ok  []           ~~ 0,  'empty array ~~ 0';
ok  [1, 2, 3]    ~~ 3.0, 'array ~~ Num (3.0)';
nok [1, 2, 3]    ~~ 3.5, 'array !~~ 3.5';

# The whole-array-of-6 smart-match from the advent 2010 day 12 medley.
ok  [1, 10, 1, 20, 1, 30] ~~ 6, '6-elem array ~~ 6';

# Existing non-numeric smart-matches are unaffected.
ok  [1, 2, 3] ~~ [1, 2, 3], 'array ~~ array still structural';
ok  "6"       ~~ 6,         'string ~~ number still numifies';

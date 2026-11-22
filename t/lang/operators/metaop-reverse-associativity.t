use Test;

plan 15;

# The `R` metaoperator reverses its base operator's associativity: `R-` and
# `R/` are right-associative, and a reversed operator binds the operand nearest
# to its left at its level, as in rakudo (#18907).

is 1 R- 2 R- 3, 0, '1 R- 2 R- 3 is 1 R- (2 R- 3)';
is 10 R- 3 R- 1, -12, '10 R- 3 R- 1';
is 2 R- 3 R- 4 R- 5, -4, 'a longer chain folds to the right';
is 8 R/ 4 R/ 2, 0.0625, '8 R/ 4 R/ 2';
is 6 R% 20 R% 7, 1, 'R% at the multiplicative level';
is 1 [R-] 2 [R-] 3, 0, 'the bracketed spelling';
is 1 R- 2 [R-] 3, 0, 'mixed spellings';

is 1 + 2 R- 3, 2, 'a reversed operator binds the nearest left operand';
is 1 R- 2 + 3, 4, 'a left-associative operator after it groups to the left';
is 8 R/ 4 * 2, 1, 'the same at the multiplicative level';
is 2 * 3 R- 4, -2, 'a tighter level stays an operand';
is 2 R- 10, 8, 'a single reversed operator';

is 2 R** 3 R** 2, 512, 'R** (already reversed)';
is 3 R** 2 R** 2, 256, 'R** groups to the left';
is (1 R~ 2 R~ 3), '321', 'R~';

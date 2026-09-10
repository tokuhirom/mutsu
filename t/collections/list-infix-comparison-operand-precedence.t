use Test;

# operators.rakudoc: the "List infix" level (Z, X, meta-ops, the sequence op)
# is LOOSER than the comma operator and thus looser than EVERY tighter operator
# (comparison ==/eq/<, tight-and &&, tight-or || min max, item assignment =,
# loose unary so/not, junctions, ...). So both operands of Z/X absorb the whole
# tighter expression: `1 == 1 Z 2 == 2` is `(1 == 1) Z (2 == 2)`, NOT the mutsu
# legacy misparse `1 == (1 Z 2) == 2`.

plan 11;

# Comparison operands (==, eq, <=, >=)
is (1 == 1 Z 2 == 2).Str,   'True True',  'Z is looser than == (both operands)';
is (1 == 2 Z 3).Str,        'False 3',    'Z looser than == on the left only';
is (1 Z 2 == 3).Str,        '1 False',    'Z looser than == on the right only';
is (1 <= 2 Z 3 >= 2).Str,   'True True',  'Z looser than <= / >=';
is (1 eq 1 Z 2 eq 2).Str,   'True True',  'Z looser than eq';

# Cross meta with comparison operands
is (1 == 2 X 3 == 4).Str,   'False False','X is looser than ==';

# Tight-and / tight-or / min-max operands
is (1 && 2 Z 3 && 4).Str,   '2 4',        'Z looser than &&';
is (1 || 0 Z 0 || 1).Str,   '1 1',        'Z looser than ||';
is (0 min 5 Z 3 min 9).Str, '0 3',        'Z looser than min';

# Loose unary so
is (so 0 Z so 1).Str,       'False True', 'Z looser than so';

# List-associative chain of same operator
is (1 == 1 Z 2 == 2 Z 3 == 3).Str, 'True True True', 'Z chains list-associatively over comparisons';

# NOTE: the item-assignment case `$x = 1 Z 2` (raku: `($x = 1) Z 2`) is NOT yet
# fixed. The statement/paren-level scalar assignment RHS is parsed by the
# dedicated `try_assign` machinery (via `expression_no_sequence`), which still
# absorbs the trailing list-infix; making that RHS parse at the item level is
# entangled with the separate word-logical-in-RHS precedence question, so it is
# deferred. The expression-level `assign_not_expr_mode` path IS fixed.

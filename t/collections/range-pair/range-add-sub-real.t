use Test;

# `Range +/- Real` must shift both endpoints, preserving exclusivity, and
# collapse to a canonical integer Range when both endpoints stay Int (so the
# result is `eqv` to a literal range). Previously only `Range + Int` worked;
# `Range + Num/Rat` and `GenericRange +/- Real` fell through to numification.

plan 16;

# Int offset stays a canonical integer Range
is-deeply (2..4)   + 2, 4..6,    'inclusive Range + Int';
is-deeply (2..^5)  + 1, 3..^6,   'exclusive-end Range + Int';
is-deeply (2^..5)  + 1, 3^..6,   'exclusive-start Range + Int';
is-deeply (2^..^5) + 1, 3^..^6,  'exclusive-both Range + Int';
is-deeply 2 + (2..4),   4..6,    'Int + Range (commutative)';
is-deeply (2..4)   - 1, 1..3,    'inclusive Range - Int';
is-deeply (2..^5)  - 1, 1..^4,   'exclusive-end Range - Int';

# Rat offset produces Rat endpoints
is-deeply (2..4)   + 0.5, 2.5..4.5,   'Range + Rat';
is-deeply 0.5 + (2..4),   2.5..4.5,   'Rat + Range (commutative)';
is-deeply (2..4)   - 0.5, 1.5..3.5,   'Range - Rat';
is-deeply (2..^4)  + 0.5, 2.5..^4.5,  'exclusive-end Range + Rat';

# Num offset produces Num endpoints
is-deeply (2..4) + 5e-1, 2.5e0..4.5e0, 'Range + Num';

# Offsetting a GenericRange (non-Int endpoints) keeps shifting
is-deeply (2.5..4.5) + 1, 3.5..5.5, 'GenericRange + Int';
is-deeply (2.5..4.5) - 1, 1.5..3.5, 'GenericRange - Int';

# `Real - Range` numifies the Range (NOT a range), matching Raku
is 0.5 - (2..4), -2.5, 'Real - Range numifies';

# role mixin is preserved across the offset
my role Meows {}
my $r := (2..^5) but Meows;
ok ($r + 1) ~~ Meows, 'role preserved through Range + Int';

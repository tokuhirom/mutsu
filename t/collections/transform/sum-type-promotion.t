use Test;

# .sum folds with `+`, so the result type promotes like Raku's reduction
# (Int+Rat -> Rat, allomorphs unwrap to their numeric value) instead of
# always collapsing to Num / truncating to Int.
plan 14;

# Allomorphs (RatStr from <...>) must not be truncated to their Int part.
is <1.5 2.5>.sum, 4, 'sum of RatStr allomorphs is exact';
is <1.5 2.5>.sum.^name, 'Rat', 'sum of RatStr stays Rat';
is <1 2 3>.sum, 6, 'sum of IntStr allomorphs';
is <1 2 3>.sum.^name, 'Int', 'sum of IntStr stays Int';

# Rat type is preserved.
is (1.5, 2.5).sum.^name, 'Rat', 'Rat sum stays Rat';
is (1, 2.5, 3).sum.^name, 'Rat', 'Int+Rat sum is Rat';
is (1/3, 1/3, 1/3).sum, 1, 'Rat sum is exact';
is (1/3, 1/3, 1/3).sum.^name, 'Rat', 'Rat sum name';

# Num stays Num, Int stays Int.
is (1.5e0, 2.5e0).sum.^name, 'Num', 'Num sum stays Num';
is (1, 2.5e0).sum.^name, 'Num', 'Int+Num sum is Num';
is (1, 2, 3).sum.^name, 'Int', 'Int sum stays Int';

# Empty list sums to Int 0.
is ().sum, 0, 'empty sum is 0';
is ().sum.^name, 'Int', 'empty sum is Int';

# BigInt promotion on overflow still works.
is (2**63, 2**63).sum, 18446744073709551616, 'sum promotes to BigInt on overflow';

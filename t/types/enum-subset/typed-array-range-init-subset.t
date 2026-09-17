use Test;

plan 6;

# Date::Utils ecosystem distribution: `subset DoW of Int is export where { 0 <
# $_ < 8 }; ... my DoW @dows = 1..7;`. Before this fix, assigning a finite
# Range to a `@`-sigilled typed array took a shortcut that spot-checked the
# element constraint against a fixed sentinel value (0 for an int Range)
# instead of the Range's actual elements -- so a subset whose `where` clause
# excludes that sentinel (as `0 < $_ < 8` excludes 0) rejected the WHOLE
# assignment even though every element of the Range legitimately satisfies
# the subset.
subset DoW of Int where { 0 < $_ < 8 };

my DoW @dows = 1..7;
is-deeply @dows, Array[DoW].new(1, 2, 3, 4, 5, 6, 7),
    'a finite Range assigned to a subset-typed array checks real elements, not a sentinel';

dies-ok { my DoW @bad = 0..7 }, 'a Range containing an out-of-subset element still dies';

# A plain (non-subset) typed array from a Range still works.
my Int @ints = 1..5;
is-deeply @ints, Array[Int].new(1, 2, 3, 4, 5), 'Int @a = Range still works';

# A Range over a type whose subset excludes an endpoint but not the whole
# Range (regression guard: only the actually-offending values should die).
subset Positive of Int where * > 0;
my Positive @pos = 1..3;
is-deeply @pos, Array[Positive].new(1, 2, 3), 'Positive @a = 1..3 (no zero in range)';
dies-ok { my Positive @neg = 0..3 }, 'Positive @a = 0..3 dies (0 is not Positive)';

# Non-integer (generic) Range endpoints are also reified per-element.
subset SingleChar of Str where *.chars == 1;
my SingleChar @letters = 'a'..'e';
is-deeply @letters, Array[SingleChar].new('a', 'b', 'c', 'd', 'e'),
    'a Str Range assigned to a subset-typed array checks real (single-char) elements';

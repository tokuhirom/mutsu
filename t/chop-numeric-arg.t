use v6;
use Test;

# `.chop($n)` coerces its argument like `.Int` (truncating toward zero),
# so a Rat/Num/allomorph count works, not only a plain Int.

plan 9;

# Rat argument truncates toward zero: 3.6 chops 3 chars.
is "Whateverable".chop(3.6), "Whatevera", "chop(Rat) truncates toward zero";

# A string that looks like an integer coerces.
is "Whateverable".chop("3"), "Whatevera", "chop(Str) parses an integer";

# A numeric allomorph coerces via its inner value.
is "Whateverable".chop(<3>), "Whatevera", "chop(IntStr) uses the inner value";

# A fractional count below 1 truncates to 0 (chops nothing).
is "Whateverable".chop(2/3), "Whateverable", "chop(2/3) truncates to 0";

# Explicit 0 and the no-arg default (1) still work.
is "Whateverable".chop(0), "Whateverable", "chop(0) removes nothing";
is "Whateverable".chop, "Whateverabl", "chop with no arg removes one char";

# A plain Int still works.
is "Whateverable".chop(3), "Whatevera", "chop(Int) removes three chars";

# Larger-than-length counts empty the string, never underflow.
is "abc".chop(10), "", "chop past the end yields the empty string";

# A Num argument truncates toward zero as well (2.9 -> 2).
is "Whateverable".chop(2.9e0), "Whateverab", "chop(Num) truncates toward zero";

done-testing;

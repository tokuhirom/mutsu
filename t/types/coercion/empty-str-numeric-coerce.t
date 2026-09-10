use Test;

plan 14;

# An empty or whitespace-only string coerces to 0 for .Int / .Num / .UInt,
# consistent with .Numeric (the string is defined-but-empty, so no failure).
is "".Int, 0, 'empty string .Int is 0';
is "   ".Int, 0, 'whitespace-only string .Int is 0';
is "\t\n".Int, 0, 'tab/newline string .Int is 0';
is "".Int.WHAT.raku, 'Int', 'empty string .Int is an Int';

is "".Num, 0, 'empty string .Num is 0';
is "  ".Num, 0, 'whitespace-only string .Num is 0';
is "".Num.WHAT.raku, 'Num', 'empty string .Num is a Num';

is "".UInt, 0, 'empty string .UInt is 0';
is "  ".UInt, 0, 'whitespace-only string .UInt is 0';

# Consistency with .Numeric and prefix + (which already returned 0).
is "".Numeric, 0, 'empty string .Numeric is 0';
is (+""), 0, 'prefix + on empty string is 0';

# Genuinely non-numeric strings still fail (a Failure, not silently 0).
ok "abc".Int ~~ Failure, 'non-numeric .Int still produces a Failure';
is "5".Int, 5, 'a valid numeric string still coerces';
is "3.14".Num, 3.14e0, 'a valid float string still coerces';

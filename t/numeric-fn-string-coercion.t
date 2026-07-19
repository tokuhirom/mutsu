use Test;

# A numeric Str coerces to its Numeric value before a Cool numeric *function*
# runs, so `sqrt "4"` is 2 and `floor "4.5"` is 4 — matching the method form
# (`f("x")` == `"x".f`). Regression: sqrt/floor/ceiling/round function arms
# matched scalar variants and fell to NaN/0 for a Str argument.

plan 16;

# sqrt / floor / ceiling / round on integer & decimal strings
is sqrt("4"),        2,  'sqrt "4" -> 2';
is sqrt("4.0"),      2,  'sqrt "4.0" -> 2';
is floor("4"),       4,  'floor "4" -> 4';
is floor("4.7"),     4,  'floor "4.7" -> 4';
is floor("-2.5"),   -3,  'floor "-2.5" -> -3';
is ceiling("4"),     4,  'ceiling "4" -> 4';
is ceiling("4.2"),   5,  'ceiling "4.2" -> 5';
is round("4.6"),     5,  'round "4.6" -> 5';
is round("4.4"),     4,  'round "4.4" -> 4';

# function form matches the method form
is sqrt("9"),   "9".sqrt,   'sqrt sub matches method on string';
is floor("7.8"), "7.8".floor, 'floor sub matches method on string';

# still-correct cases (regression guard)
is abs("-5"),   5, 'abs "-5" -> 5';
is abs("6+8i"), 10, 'abs "6+8i" -> 10 (Complex string)';
is sign("4"),   1, 'sign "4" -> 1';

# a string function is NOT hijacked by numeric coercion
is chars("6+8i"), 4, 'chars "6+8i" -> 4 (string length)';
is chars("123"),  3, 'chars "123" -> 3 (string length)';

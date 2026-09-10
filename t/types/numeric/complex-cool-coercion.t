use Test;

# Two Complex/Cool coercion fixes surfaced by the QA doc-diff harness:
#
#  1. Complex.isNaN is True when EITHER part is NaN (Type/Complex.rakudoc).
#  2. A Str that numifies to a Complex/Rat coerces fully before a Cool numeric
#     method/function runs, so `abs "6+8i"` is 10 and `"1+2i".conj` is 1-2i
#     (Type/Cool.rakudoc). Previously the numeric arms matched scalar variants
#     only and fell to a 0/NaN default for such a Str.

plan 16;

# --- Complex.isNaN ---
ok  (NaN + 5i).isNaN,        'isNaN: NaN real part';
ok  (7 + NaN\i).isNaN,       'isNaN: NaN imaginary part';
ok  (NaN + NaN\i).isNaN,     'isNaN: both parts NaN';
nok (7 + 5i).isNaN,          'isNaN: finite Complex is not NaN';
nok (0 + 0i).isNaN,          'isNaN: zero Complex is not NaN';

# --- Str -> Complex coercion, method form ---
is "6+8i".abs, 10,           '"6+8i".abs -> 10';
is "3-4i".abs, 5,            '"3-4i".abs -> 5';
is "1+2i".conj, 1 - 2i,      '"1+2i".conj -> 1-2i';
is "6+8i".re, 6,             '"6+8i".re -> 6';
is "6+8i".im, 8,             '"6+8i".im -> 8';

# --- Str -> Complex coercion, function form ---
is abs("6+8i"), 10,          'abs "6+8i" -> 10';
is abs("3-4i"), 5,           'abs "3-4i" -> 5';

# --- Str -> Rat coercion still works ---
is abs("1/2"), 0.5,          'abs "1/2" -> 0.5 (Rat coercion)';
is abs("-3/4"), 0.75,        'abs "-3/4" -> 0.75';

# --- plain integer/float strings unaffected; string ops not hijacked ---
is abs("-5"), 5,             'abs "-5" -> 5 (plain int string)';
is chars("6+8i"), 4,         'chars "6+8i" -> 4 (string length, NOT coerced)';

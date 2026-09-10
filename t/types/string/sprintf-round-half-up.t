use Test;

# Rakudo rounds %f half-away-from-zero (not half-to-even), and uses the exact
# value of Rat literals / the shortest-decimal value of Num, so the results are
# stable regardless of f64 representation quirks.

plan 22;

# Half-away-from-zero on exactly-representable halves (Rat literals)
is sprintf("%.0f", 0.5),  "1", "0.5 rounds up";
is sprintf("%.0f", 1.5),  "2", "1.5 rounds up";
is sprintf("%.0f", 2.5),  "3", "2.5 rounds up (not banker's 2)";
is sprintf("%.0f", 3.5),  "4", "3.5 rounds up";
is sprintf("%.0f", 4.5),  "5", "4.5 rounds up (not banker's 4)";

# Negative halves round away from zero
is sprintf("%.0f", -0.5), "-1", "-0.5 rounds away from zero";
is sprintf("%.0f", -2.5), "-3", "-2.5 rounds away from zero";

# Below/above the half do not flip
is sprintf("%.0f", 0.25), "0", "0.25 rounds down";
is sprintf("%.0f", 0.35), "0", "0.35 rounds down";

# Rat literals are exact: 2.675 == 2675/1000, so it rounds to 2.68
is sprintf("%.2f", 2.675), "2.68", "exact Rat 2.675 -> 2.68";
is sprintf("%.2f", 0.125), "0.13", "exact 0.125 -> 0.13";
is sprintf("%.2f", 1.005), "1.01", "exact Rat 1.005 -> 1.01";

# Num (float) arguments use shortest-decimal value, same results
is sprintf("%.0f", 2.5e0),   "3",    "Num 2.5e0 -> 3";
is sprintf("%.2f", 1.005e0), "1.01", "Num 1.005e0 -> 1.01";
is sprintf("%.2f", 0.125e0), "0.13", "Num 0.125e0 -> 0.13";

# Sign preservation
is sprintf("%.2f", -0.0e0), "-0.00", "negative zero keeps its sign";
is sprintf("%.2f", 0.0e0),  "0.00",  "positive zero has no sign";

# Integers and flags still work
is sprintf("%.2f", 100),    "100.00", "Int argument";
is sprintf("%+.2f", 3.14),  "+3.14",  "plus flag";
is sprintf("%08.2f", 3.14), "00003.14", "zero-pad width";

# String numeric arguments
is sprintf("%.2f", "3.14159"), "3.14", "string numeric argument";
is sprintf("%.0f", "2.5"),     "3",    "string half rounds up";

done-testing;

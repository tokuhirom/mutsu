use Test;
plan 8;

is defined(42), True, "defined int";
is defined(Nil), False, "defined Nil";

is sqrt(16), 4, "sqrt function";
is floor(3.7), 3, "floor function";
is ceiling(3.2), 4, "ceiling function";
is round(3.5), 4, "round function";

is min(3, 1, 2), 1, "min function";
is max(3, 1, 2), 3, "max function";

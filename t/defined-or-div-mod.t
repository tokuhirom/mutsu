use Test;
plan 6;

is (Nil // 42), 42, "// Nil gives rhs";
is (5 // 42), 5, "// defined gives lhs";
is (0 // 42), 0, "// falsy but defined gives lhs";

is (7 div 2), 3, "div integer division";
is (7 mod 3), 1, "mod remainder";
is (-7 mod 3), 2, "mod with negative (Euclidean)";

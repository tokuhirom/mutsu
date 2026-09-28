use Test;

plan 12;

# Rakudo's min/max skip an undefined operand (a type object like `Int`, not
# just the bare `Any`), and every form -- the sub, the infix operator, and
# the `.min`/`.max` methods -- must agree (ADR-0117/0118: one implementation).
is min(2, Int), 2, "min(2, Int) skips the undefined Int operand";
is 2 min Int, 2, "infix min skips the undefined Int operand";
is (2, Int).min, 2, ".min skips the undefined Int operand";
is max(2, Int), 2, "max(2, Int) skips the undefined Int operand";
is 2 max Int, 2, "infix max skips the undefined Int operand";
is (2, Int).max, 2, ".max skips the undefined Int operand";

is min(2, Str), 2, "min(2, Str) skips a different undefined type object";
is max(2, Str), 2, "max(2, Str) skips a different undefined type object";

# A slurpy call with more than two arguments takes a different code path
# than the two-argument sub form -- both must skip.
is min(2, Int, 5), 2, "min with 3 args skips the undefined middle operand";
is max(2, Int, 5), 5, "max with 3 args skips the undefined middle operand";

# When every operand is undefined, one of them is still returned (no crash).
ok !min(Int, Int).defined, "min(Int, Int) returns an undefined value, not a crash";
ok !max(Int, Int).defined, "max(Int, Int) returns an undefined value, not a crash";

use v6;
use Test;

plan 9;

# A 1-arity .sort block that returns a *list* of keys is a multi-key
# Schwartzian sort: keys compare element-wise (Raku list cmp), the first
# differing element decides, shorter-as-prefix sorts Less.
is <01 11 111 2 20 02>.sort( { .Int, .comb.sum, .Str } ).join(" "),
   "01 02 2 11 20 111",
   "multi-key sort by (.Int, .comb.sum, .Str)";

# List cmp / <=> semantics that the same code path relies on.
is (1, 2, 3) cmp (1, 2, 4), Less, "list cmp: first difference decides";
is (1, 2)    cmp (1, 2, 3), Less, "list cmp: prefix sorts Less";
is (1, 2, 3) cmp (1, 2),    More, "list cmp: longer sorts More";
is ("a", "b") cmp ("a", "c"), Less, "list cmp: string elements";
is (2,) cmp (10,), Less, "list cmp: numeric, not stringwise";
is (1, 2, 3) cmp (1, 2, 3), Same, "list cmp: equal lists are Same";

# Multi-key sort over pairs (.key primary, .value secondary).
my @data = (3 => 'c'), (1 => 'a'), (3 => 'a'), (1 => 'b');
is @data.sort({ .key, .value }).gist,
   "(1 => a 1 => b 3 => a 3 => c)",
   "multi-key sort of pairs by (.key, .value)";

# Numeric keys must compare numerically inside the list, not stringwise.
is (2, 10, 1, 20).map({ ($_,) }).sort.map(*.[0]).join(" "),
   "1 2 10 20",
   "singleton-list keys still sort numerically";

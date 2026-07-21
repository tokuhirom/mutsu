use v6;
use Test;

# `Str.contains($needle, :ignoremark)` compares after stripping combining marks
# (NFD, dropping combining code points); `:ignorecase` folds case. They combine.

plan 10;

is "abc".contains("ä"), False, "plain contains: a-with-diaeresis absent";
is "abc".contains("ä", :ignoremark), True, ":ignoremark strips the needle's mark";
is "äbc".contains("a", :ignoremark), True, ":ignoremark strips the haystack's mark";
is "café".contains("cafe", :ignoremark), True, ":ignoremark over a full word";
is "café".contains("cafx", :ignoremark), False, ":ignoremark still needs the rest to match";

is "ABC".contains("b", :ignorecase), True, ":ignorecase folds case";
is "ÀBC".contains("àb", :ignorecase, :ignoremark), True, ":ignorecase + :ignoremark combine";
is "ÀBC".contains("àb", :ignoremark), False, ":ignoremark alone keeps case significant";

is "abc".contains("x", :ignoremark), False, "absent needle stays False under :ignoremark";
is "abc".contains("B", :i), True, ":i shorthand for :ignorecase";

done-testing;

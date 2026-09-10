use v6;
use Test;

# index() / .index with a *list* of needles returns the smallest index at which
# any of the needles matches (rakudo issue #6104). rindex already supported this;
# the sub form of index used to stringify the whole list and never match.

plan 18;

# --- sub form: index($str, @needles) ---------------------------------------
is index("ab",  <ab b>),  0, "second needle contained in the first";
is index("ab",  <b ab>),  0, "first needle contained in the second";
is index("abc", <ab bc>), 0, "partial overlap, small index first";
is index("abc", <bc ab>), 0, "partial overlap, large index first";
is index("abcbc", <bc abc>), 0, "needle contained in another, twice in str";
is index("abccbccbab", <ccbc bccb cbcc bcc bab ccbc>), 1, "many overlapping needles";
is index("hello world", <world lo o>), 3, "earliest of several matches";

# no match across all needles -> undefined
nok index("aaa", <x y z>).defined, "no needle matches -> undefined (sub)";

# a single-element list still works
is index("abc", <bc>), 1, "single-element needle list";

# start position is honoured with a needle list
is index("abcabc", <bc>, 2), 4, "list needle with start position";

# --- method form: $str.index(@needles) -------------------------------------
is "abc".index(<bc ab>), 0, "method form: earliest match";
is "abccbccbab".index(<ccbc bccb cbcc bcc bab ccbc>), 1, "method form: many overlapping";
nok "aaa".index(<x y>).defined, "method form: no match -> undefined";

# --- scalar needle unchanged (regression guard) ----------------------------
is index("abc", "bc"), 1, "scalar needle still works (sub)";
is "abc".index("bc"), 1, "scalar needle still works (method)";
nok index("abc", "xyz").defined, "scalar no-match still undefined";

# --- rindex list form (already worked; guard it) ---------------------------
is rindex("abcbc", <bc abc>), 3, "rindex list form: rightmost match";
is rindex("abccbccbab", <ccbc bccb cbcc bcc bab ccbc>), 7, "rindex list: many overlapping";

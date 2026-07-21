use v6;
use Test;

# `Str.contains($needle)` accepts a Regex needle: it asks whether the pattern
# matches anywhere (from an optional start position), not whether the string
# literally contains the regex's gist.

plan 11;

is "Hello, World".contains(/o/), True, "regex needle matches";
is "Hello, World".contains(/z/), False, "regex needle that never matches";

# With a start position, the search begins at that character index.
is "Hello, World".contains(/o/, 5), True, "regex needle from a start position";
is "abcabc".contains(/b/, 3), True, "regex needle finds a later occurrence";
is "abcabc".contains(/a/, 4), False, "regex needle: nothing after the start pos";

# Zero-width lookahead assertions work (doc example).
is 'Hello, World'.contains(/\w <?before ','>/), True, "lookahead assertion matches";
is 'Hello, World'.contains(/\w <?before ','>/, 5), False, "lookahead assertion from pos 5";

# A plain string needle still works.
is "abc".contains("b"), True, "string needle still works";
is "abc".contains("z"), False, "string needle absent";

# A regex with a quantifier / char class.
is "a1b2c3".contains(/\d\d/), False, "no two consecutive digits";
is "a12b".contains(/\d\d/), True, "two consecutive digits present";

done-testing;

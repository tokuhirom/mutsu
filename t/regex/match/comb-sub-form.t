use v6;
use Test;

# `comb` has a sub form: `comb($matcher, $input, $limit?)` == `$input.comb($matcher, $limit)`.
# Previously a bare `comb(...)` call aborted at compile time with
# "Undeclared routine: comb".

plan 8;

is comb(/\w/, "a;b;c").raku, ("a", "b", "c").Seq.raku, "comb(regex, str)";
is comb(/\N/, "a;b;c").raku, ("a", ";", "b", ";", "c").Seq.raku, "comb(regex, str) non-word";
is comb(/\w/, "a;b;c", 2).raku, ("a", "b").Seq.raku, "comb(regex, str, limit)";
is comb("a", "banana").raku, ("a", "a", "a").Seq.raku, "comb(str, str)";
is comb(2, "abcdefg").raku, ("ab", "cd", "ef", "g").Seq.raku, "comb(size, str)";
is comb(3, "abcdefghijk").raku, ("abc", "def", "ghi", "jk").Seq.raku, "comb(size, str) uneven";

# The sub form returns a Seq, like the method.
is comb(/\w/, "a;b;c").^name, "Seq", "comb sub returns a Seq";

# Limit of 0 yields an empty Seq.
is comb(/\w/, "a;b;c", 0).elems, 0, "comb with a 0 limit is empty";

done-testing;

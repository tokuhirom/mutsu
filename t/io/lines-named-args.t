use v6;
use Test;

# `lines` / `Str.lines` accept the `:chomp` and `:count` named args, and the
# sub form accepts a named arg before the string positional.

plan 9;

# `:count` (method form) returns the number of lines instead of the list.
is "a\nb\nc".lines(:count), 3, ".lines(:count) returns the line count";
is "".lines(:count), 0, ".lines(:count) on an empty string is 0";
is "one line".lines(:count), 1, ".lines(:count) with no newline is 1";

# `:count` (sub form).
is lines("a\nb\nc", :count), 3, "lines(str, :count) returns the count";

# A named arg may precede the string in the sub form.
is lines(:!chomp, "a\nb").raku, ("a\n", "b").Seq.raku,
    "lines(:!chomp, str) keeps line endings";
is lines("a\nb").raku, ("a", "b").Seq.raku, "lines(str) chomps by default";

# `:chomp` (method form) still works.
is "a\nb".lines(:!chomp).raku, ("a\n", "b").Seq.raku, ".lines(:!chomp) keeps endings";
is "a\n".lines(:!chomp).elems, 1, ".lines(:!chomp) on a trailing newline";

# `.lines` with no args is unaffected.
is "a\nb".lines.elems, 2, "plain .lines still splits";

done-testing;

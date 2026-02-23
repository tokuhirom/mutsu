use Test;
plan 12;

is flip("hello"), "olleh", "flip function";
is lc("HELLO"), "hello", "lc function";
is uc("hello"), "HELLO", "uc function";
is tc("hello"), "Hello", "tc function";
is chomp("hello\n"), "hello", "chomp function";
is chop("hello"), "hell", "chop function";
is trim("  hi  "), "hi", "trim function";

my @w = words("hello world foo");
is @w.elems, 3, "words function count";

is substr("hello", 1, 3), "ell", "substr function";
is index("hello", "ll"), 2, "index function";
is samemark("zoo", "ŏôō"), "z̆ôō", "samemark function";
is samemark("foo", ""), "foo", "samemark empty source";

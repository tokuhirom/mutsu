use Test;

plan 9;

# A class that starts with a negated part is "every character except that",
# so a later `+` part is a union with the complement (issue #9907).
is ("a" ~~ /<-[\s] + [x]>/).Str, "a", '<-[\s] + [x]> matches a non-space';
is ("a" ~~ /<-[b] + [c]>/).Str, "a", '<-[b] + [c]> matches a';
is ("x" ~~ /<-[\s] + [x]>/).Str, "x", 'the right operand still matches';
nok ("b" ~~ /<-[b] + [c]>/).Bool, 'the negated char stays excluded';
is ("c" ~~ /<-[b] + [c]>/).Str, "c", 'the positive part matches';
is ("c" ~~ /<-[b] + [c] - [a]>/).Str, "c", 'a trailing subtraction keeps c';
nok ("a" ~~ /<-[b] + [c] - [a]>/).Bool, 'a trailing subtraction removes a';
is ("aab" ~~ /<-[b] + [c]>+/).Str, "aa", 'quantified union';
is ("a" ~~ /<-[b] - [c]>/).Str, "a", 'subtraction form is unchanged';

use Test;

# An unrecognized alphabetic backslash sequence inside an interpolating
# (double-quoted / qq) string is a compile-time
# X::Backslash::UnrecognizedSequence error.

plan 9;

throws-like '"\u"', X::Backslash::UnrecognizedSequence, sequence => 'u';
throws-like '"\d"', X::Backslash::UnrecognizedSequence, sequence => 'd';
throws-like '"\w"', X::Backslash::UnrecognizedSequence, sequence => 'w';
throws-like '"a\hb"', X::Backslash::UnrecognizedSequence, sequence => 'h';

# Recognized escapes still work.
is "a\nb", "a\x[0A]b", 'recognized \n still works';
is "a\tb".chars, 3, 'recognized \t still works';

# Non-alphanumeric backslash escapes produce themselves (no error).
is "a\.b", 'a.b', 'backslash before . is literal .';
is "a\/b", 'a/b', 'backslash before / is literal /';

# Single-quoted strings keep an unknown backslash sequence literal.
is '\u', "\\u", 'single-quoted \u stays literal';

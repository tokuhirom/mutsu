use Test;

# A defined non-Regex/non-Str matcher passed to `.match` is coerced to its
# string form and matched literally (see Type/Str.rakudoc):
#   "1 2 3".match([1,2,3])  matches [1,2,3].Str  == "1 2 3"
#   "x123y".match(123)      matches "123"
# An *undefined* matcher (Nil / a bare type object) still throws X::Multi::NoMatch.

plan 8;

is "1 2 3".match([1,2,3]), '1 2 3', 'a list matcher is stringified (space-joined)';
is "x123y".match(123), '123', 'an Int matcher matches its decimal string';
is "5".match(5), '5', 'a single-digit Int matcher';
is "abc".match(97), Nil, 'an Int whose string is absent yields Nil';

# A list is joined with spaces, so a run without the spaces does not match.
is "123".match([1,2,3]), Nil, 'list matcher keeps the space separators';

# A Rat matcher stringifies too.
is "pi=3.5!".match(3.5), '3.5', 'a Rat matcher matches its string form';

# The matcher sets $/ like any other match.
"x123y".match(123);
is ~$/, '123', 'a coerced literal match still populates $/';

# Undefined matchers remain a multi-dispatch failure.
dies-ok { "abc".match(Any) }, 'a bare type object matcher dies (X::Multi::NoMatch)';

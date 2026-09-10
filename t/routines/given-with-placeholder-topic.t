use v6;
use Test;

# A scalar placeholder in a given/with body is the block's parameter, bound
# to the TOPIC — not to the enclosing block's condition value (`with EXPR`
# desugars through a defined-check whose Bool must not leak into $^a).
# Base64's `do with (3 - ($c.key+1) % 3) { $^a == 3 ?? 0 !! $^a }`.

plan 4;

is (do with 2 { $^a }), 2, 'with binds the placeholder to the topic';
is (do with 2 { $^a == 3 ?? 0 !! $^a }), 2,
    'ternary over the placeholder sees the topic';
is (do given 5 { $^a + 1 }), 6, 'given binds the placeholder to the topic';
is (do with "text" { $^v.chars }), 4, 'string topic binds too';

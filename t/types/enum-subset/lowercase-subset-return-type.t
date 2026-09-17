use Test;

plan 3;

# #8657: a `-->` spec naming a lowercase-named user `subset` is a type
# constraint, not a definite return value -- exactly like an uppercase-named
# subset (`subset MySubset of UInt where * < 100;` never reproduced this).
# `return $x` inside the body must be allowed, and the value must actually
# flow through `return`, not get treated as a bogus "definite return value
# already specified in the signature" conflict.
#
# Verified against `raku`: a lowercase subset must be declared before the
# sub that uses it as a return type (referencing it earlier is itself a
# compile error in real Raku, "Type '...' is not declared" -- unrelated to
# this bug), so all cases below declare the subset first.

subset ipv6_int8657 of UInt where * < 100;

sub foo8657(Str:D $ip --> ipv6_int8657) {
    return 42;
}
is foo8657("x"), 42, 'lowercase subset return type allows an explicit `return`';

throws-like
    { sub baz8657(--> ipv6_int8657) { return 200 }; baz8657() },
    X::TypeCheck::Return,
    'a lowercase-subset return type constraint still rejects a mismatched return value';

subset ipv4_8657 of UInt where * < 256;
my sub qux8657(--> ipv4_8657) { 7 }
is qux8657(), 7, 'the implicit final expression still flows through a lowercase subset return type';

use v6;
use Test;

# A Hash coerces to a Set of the keys whose values are truthy (the same rule
# `.Set` uses). So set-membership operators (`∈`/`(elem)`, `∋`/`(cont)` and
# their negations) treat a key mapped to a falsy value (0, Nil, "") as NOT a
# member. Previously mutsu tested bare key existence, ignoring the value.

plan 10;

# The doc trap: enum values start at 0, so `a => 0` is a falsy pair and `a` is
# not in the enum's Set.
{
    enum Foo «a b»;
    nok Foo.enums ∋ 'a', 'enum key with value 0 is not a set member';
    ok  Foo.enums ∋ 'b', 'enum key with value 1 is a set member';
}

# Direct hash membership honors value truthiness.
ok  %(a => 1, b => 0) ∋ 'a', 'key with truthy value is a member (cont)';
nok %(a => 1, b => 0) ∋ 'b', 'key with value 0 is not a member (cont)';
ok  'a' ∈ %(a => 1, b => 0), 'truthy key is an element (elem)';
nok 'b' ∈ %(a => 1, b => 0), 'value-0 key is not an element (elem)';

# Falsy string/undef values also exclude the key.
nok %(a => "") ∋ 'a', 'empty-string value is not a member';
nok %(a => Nil) ∋ 'a', 'Nil value is not a member';

# Negated operators flip correctly.
ok %(a => 1, b => 0) ∌ 'b', 'value-0 key satisfies not-contains';
ok 'b' ∉ %(a => 1, b => 0), 'value-0 key satisfies not-element';

use Test;

# A negated colonpair may not carry an argument: `:!foo(3)` is a compile-time
# X::Syntax::NegatedPair carrying the offending key.

plan 4;

throws-like ':!foo(3)', X::Syntax::NegatedPair, key => 'foo';
throws-like ':!bar(1, 2)', X::Syntax::NegatedPair, key => 'bar';

# A plain negated pair (no argument) is fine: `:!foo` means foo => False.
my %h = (:!foo, :baz);
is %h<foo>, False, ':!foo is foo => False';
is %h<baz>, True, ':baz is baz => True';

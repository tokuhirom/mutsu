use Test;

plan 4;

# A negated colonpair may not carry an argument.
throws-like ':!foo(3)', X::Syntax::NegatedPair, key => 'foo';
throws-like ':!bar(1, 2)', X::Syntax::NegatedPair, key => 'bar';
throws-like ':!some-name(42)', X::Syntax::NegatedPair, key => 'some-name';

# A bare negated pair (no argument) is legal and is False.
is (:!foo).value, False, 'bare :!foo is a False-valued pair';

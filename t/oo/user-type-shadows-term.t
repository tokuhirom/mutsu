use v6;
use Test;

# A user-declared type shadows a built-in *term constant* of the same name
# (Empty, True, False, Nil, Any). Raku resolves the lexically-declared type.
# Numeric literals (Inf, NaN) are NOT shadowable and keep their value.

plan 7;

# Unshadowed built-in terms still behave as constants
is (1, 2, Empty, 3).elems, 3, 'unshadowed Empty is the empty Slip';
ok Inf > 1e308, 'unshadowed Inf is infinity';
ok NaN != NaN, 'unshadowed NaN is not-a-number';

# A user class named Empty shadows the term
{
    class Empty { has $.v; }
    my $e = Empty.new(v => 5);
    is $e.v, 5, 'class Empty shadows the Empty term (construct)';
    is $e.^name, 'Empty', 'class Empty shadows the Empty term (^name)';
}

# Inf / NaN are numeric literals: a like-named class does NOT shadow them
{
    class Inf { method answer { 42 } }
    ok Inf > 1e308, 'Inf stays a numeric literal even with class Inf declared';
}

# A user class named Nil shadows the Nil term
{
    class Nil { method greet { 'hi' } }
    is Nil.greet, 'hi', 'class Nil shadows the Nil term';
}

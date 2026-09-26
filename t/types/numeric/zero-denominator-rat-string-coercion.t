use Test;

# GH #9621: every string-context coercion of a zero-denominator Rational dies
# like `(1/0).Str` does, not only the `.Str`/`.gist` method calls: prefix and
# infix `~`, the string comparators, interpolation (`"$x"`, `"{...}"`,
# `"@a[]"`) and `join` render through a pure stringifier that printed `Inf`.
# The exception carries Rakudo's `details` suffix.

plan 22;

my $x = 1/0;
my @a = 1/0;

throws-like { ~$x }, X::Numeric::DivideByZero, 'prefix ~';
throws-like { ~(1/0) }, X::Numeric::DivideByZero, 'prefix ~ on a literal';
throws-like { ~[$x] }, X::Numeric::DivideByZero, 'prefix ~ on an Array';
throws-like { "a" ~ $x }, X::Numeric::DivideByZero, 'infix ~';
throws-like { "a" ~ 1/0 }, X::Numeric::DivideByZero, 'infix ~ on literals (not constant-folded)';
throws-like { $x eq "x" }, X::Numeric::DivideByZero, 'infix eq';
throws-like { "$x" }, X::Numeric::DivideByZero, 'scalar interpolation';
throws-like { "{1/0}" }, X::Numeric::DivideByZero, 'block interpolation';
throws-like { "<$x>" }, X::Numeric::DivideByZero, 'interpolation among other parts';
throws-like { "@a[]" }, X::Numeric::DivideByZero, 'array interpolation';
throws-like { [$x].join(",") }, X::Numeric::DivideByZero, '.join with a separator';
throws-like { [$x].join }, X::Numeric::DivideByZero, '.join without a separator';
throws-like { (1, $x).join("-") }, X::Numeric::DivideByZero, 'List .join';
throws-like { join(",", $x) }, X::Numeric::DivideByZero, 'join listop';
throws-like { join(",", $x, 1|2) }, X::Numeric::DivideByZero, 'join listop threading a Junction';
throws-like { ~FatRat.new(1, 0) }, X::Numeric::DivideByZero, 'FatRat';

throws-like { ~$x }, X::Numeric::DivideByZero,
    message => 'Attempt to divide 1 by zero when coercing Rational to Str',
    details => 'when coercing Rational to Str',
    'message carries the numerator and the details';
throws-like { ~(0/0) }, X::Numeric::DivideByZero,
    message => 'Attempt to divide by zero when coercing Rational to Str',
    '0/0 leaves the zero numerator out of the message';

# Unaffected: a user prefix:<~> candidate, .raku, and a finite Rat.
{
    multi prefix:<~>(Rat $r) { "custom" }
    is ~$x, 'custom', 'a user prefix:<~> candidate still wins';
}
is $x.raku, '<1/0>', '.raku still renders the Rational';
is "{1/2}|" ~ 3/4, '0.5|0.75', 'a finite Rat still stringifies';
is [1/2, 3].join(","), '0.5,3', 'a finite Rat still joins';

use Test;

plan 6;

# `Instant.from-posix` numifies its argument: a string that is no number dies
# with X::Str::Numeric (App::RaCoCo's index reader wraps it in `try` to skip a
# line with a bad timestamp).
throws-like { Instant.from-posix("not-numeric-value") }, X::Str::Numeric,
    'a non-numeric string dies';
throws-like { Instant.from-posix("abc", True) }, X::Str::Numeric,
    'also with the leap-second flag';
ok (try Instant.from-posix("abc")) === Nil, 'try gives Nil';

is Instant.from-posix("5").to-posix[0], 5, 'a numeric string works';
is Instant.from-posix("1485726595.3").to-posix[0], 1485726595.3, 'a decimal string works';
is Instant.from-posix(7).to-posix[0], 7, 'a number works';

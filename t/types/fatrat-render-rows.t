use Test;

# The scalar values' `gist` and `raku` are rows (`method_table::scalars::render`),
# the zero-denominator rationals and `FatRat` included (ADR-11276 §9.152); the
# zero-argument rendering cascade answers no receiver that has a shape.

plan 16;

is FatRat.new(1, 3).gist, '0.333333', 'FatRat.gist';
is FatRat.new(1, 3).raku, 'FatRat.new(1, 3)', 'FatRat.raku';
is FatRat.new(1, 0).raku, 'FatRat.new(1, 0)', 'FatRat.raku with a zero denominator';
is FatRat.new(10**30, 3).raku, 'FatRat.new(1000000000000000000000000000000, 3)',
    'a big FatRat.raku';
is (2**70/3).raku, '<1180591620717411303424/3>', 'a big Rat.raku';
is <1/0>.raku, '<1/0>', 'Rat.raku with a zero denominator';
throws-like { <1/0>.gist }, X::Numeric::DivideByZero, 'Rat.gist with a zero denominator';
throws-like { FatRat.new(1, 0).gist }, X::Numeric::DivideByZero,
    'FatRat.gist with a zero denominator';

is 42.raku, '42', 'Int.raku';
is (2**70).gist, '1180591620717411303424', 'a big Int.gist';
is 1e20.gist, '1e+20', 'Num.gist';
is (3+4i).raku, '<3+4i>', 'Complex.raku';
is "a\"b".raku, '"a\\"b"', 'Str.raku';
is True.raku, 'Bool::True', 'Bool.raku';
is (v1.2.3).gist, 'v1.2.3', 'Version.gist';

# An `augment` of the type does not take the rendering from the rows.
use MONKEY-TYPING;
augment class Int { method shout { "!" } }
is 5.raku, '5', 'Int.raku after an augment';

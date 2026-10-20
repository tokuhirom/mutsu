use Test;

# `Real.polymod` is Rakudo's `mod`/`div` loop, not an exact/float decomposition.
plan 13;

is-deeply 10.polymod(2.5), (-2.5, 5).Seq, 'Int over a Rat: mod\'s Real candidate, div coerces to Int';
is-deeply 4.polymod(1.5), (-2.0, 4).Seq, 'a second Int/Rat case';
is-deeply 9.polymod(2.5, 2), (-1.0, 0, 2).Seq, 'the Int quotient carries into the next step';
is-deeply 10.polymod(2.5e0), (-2.5e0, 5).Seq, 'an Int over a Num divisor keeps an Int quotient';
is-deeply 2.5e0.polymod(2), (0.5e0, 1e0).Seq, 'a Num quotient stays a Num';
is-deeply 10e0.polymod(3), (1e0, 3e0).Seq, 'Num invocant, Int divisor';
is-deeply 5.5e0.polymod(2, 3), (1.5e0, 2e0, 0).Seq, 'a zero element is an Int';
is-deeply 12e0.polymod(5, 2), (2e0, 0, 1e0).Seq, 'a zero remainder is an Int';
is-deeply 7.polymod(0.5), (7,).Seq, 'a divisor that coerces to Int 0 ends the decomposition';
is-deeply 5.Rat.polymod(.3, .2), (0.2, 0, 80.0).Seq, 'a Rat invocant keeps the exact rational decomposition';
is-deeply 3661.polymod(60, 60), (1, 1, 1).Seq, 'Int over Int is unchanged';
throws-like { 7.polymod(0) }, X::Numeric::DivideByZero, using => 'polymod', 'a zero divisor throws';
is (-7e0).polymod(3).exception.message, 'invocant to polymod out of range. Is: -7, should be in 0..^Inf',
    'a negative invocant is an X::OutOfRange failure';

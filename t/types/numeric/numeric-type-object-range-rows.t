use Test;

# ADR-11276 §9.97: `.Range` of the numeric type objects is a row of `Int`,
# `Num`, `Rat`/`FatRat` and the `Rational` role. A native integer type object
# reaches `Int`'s through its MRO, and an instance's `.Range` throws
# `X::Parameter::InvalidConcreteness`. The answers below are Rakudo's.

plan 14;

is Int.Range.raku, '-Inf^..^Inf', 'Int.Range';
is Num.Range.raku, '-Inf..Inf', 'Num.Range';
is Rat.Range.raku, '-Inf..Inf', 'Rat.Range';
is FatRat.Range.raku, '-Inf..Inf', 'FatRat.Range';
is Rational.Range.raku, '-Inf..Inf', 'Rational.Range (the role)';
is RatStr.Range.raku, '-Inf..Inf', 'RatStr.Range (through Rat)';
is int8.Range.raku, '-128..127', 'int8.Range';
is uint16.Range.raku, '0..65535', 'uint16.Range';
is uint.Range.max, 18446744073709551615, 'uint.Range.max';
throws-like { Real.Range }, X::Method::NotFound, 'Real declares no Range';
throws-like { 5.Range }, X::Parameter::InvalidConcreteness,
    'an Int instance has no Range';
throws-like { 2.5.Range }, X::Parameter::InvalidConcreteness,
    message => /'type \'Rat\''/, '... nor a Rat';
throws-like { 2.5e0.Range }, X::Parameter::InvalidConcreteness,
    '... nor a Num';
throws-like { FatRat.new(1, 2).Range }, X::Parameter::InvalidConcreteness,
    '... nor a FatRat';

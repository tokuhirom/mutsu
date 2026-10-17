use Test;

# A numeric method whose invocant is `:D` refuses a type object through its
# row (ADR-11276, `RowFlags::DEFINITE`): `X::Parameter::InvalidConcreteness`
# for a single `:D` candidate, `X::Multi::NoMatch` when every candidate of a
# `multi` is `:D` (`RowFlags::DEFINITE_MULTI`), as in Rakudo.

plan 21;

for <abs sign floor ceiling truncate narrow lsb msb Rat FatRat> -> $m {
    throws-like { Int."$m"() }, X::Parameter::InvalidConcreteness,
        "Int.$m refuses the type object";
}

for <sqrt exp log log2 log10 round chr> -> $m {
    throws-like { Int."$m"() }, X::Multi::NoMatch,
        "Int.$m finds no candidate for the type object";
}

throws-like { Num.floor }, X::Parameter::InvalidConcreteness, 'Num.floor';
throws-like { UInt.abs }, X::Parameter::InvalidConcreteness, 'UInt.abs is Int.abs';
throws-like { Complex.Rat }, X::Multi::NoMatch, 'Complex.Rat';
throws-like { Int.polymod(10) }, X::Parameter::InvalidConcreteness, 'Int.polymod';

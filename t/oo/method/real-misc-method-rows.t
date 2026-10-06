use Test;

# ADR-11276 slice 3B: is-prime, narrow, conj, Bridge, lsb, msb, chr, the native
# integer coercions and rand are handler rows. Rakudo declares is-prime, conj,
# chr, rand and the coercions on Cool (`self.Numeric.METHOD`) and narrow, Bridge,
# lsb and msb on the numeric types only. Expected values are Rakudo's.

plan 10;

subtest 'is-prime', {
    plan 9;
    ok 7.is-prime, 'Int';
    nok 8.is-prime, 'a composite';
    nok (-7).is-prime, 'a negative Int';
    ok 7e0.is-prime, 'a Num with an integral value';
    nok 7.5.is-prime, 'a fractional Rat';
    ok (14/2).is-prime, 'a Rat with an integral value';
    ok "7".is-prime, 'Cool numifies a Str';
    ok [1, 2, 3].is-prime, 'and a List to its element count';
    throws-like { <1+2i>.is-prime }, X::Numeric::Real, 'a Complex with an imaginary part';
}

subtest 'narrow', {
    plan 9;
    is 4.narrow.^name, 'Int', 'Int';
    is (4/2).narrow.^name, 'Int', 'an integral Rat';
    is 2.5.narrow.^name, 'Rat', 'a fractional Rat';
    is 2e0.narrow.^name, 'Int', 'an integral Num';
    is 2.5e0.narrow.^name, 'Num', 'a fractional Num';
    is <2+0i>.narrow.^name, 'Int', 'a Complex with no imaginary part';
    is <2.5+0i>.narrow.^name, 'Num', 'and a non-integral real part';
    is 3.FatRat.narrow.^name, 'Int', 'an integral FatRat';
    throws-like { "1.5".narrow }, X::Method::NotFound, 'a Str has no narrow';
}

subtest 'conj', {
    plan 6;
    is 5.conj, 5, 'Int';
    is-deeply 1.5e0.conj, 1.5e0, 'Num';
    is (1/3).conj, (1/3), 'Rat';
    is-deeply <1+2i>.conj, <1-2i>, 'Complex';
    is-deeply "4".conj, 4, 'Cool numifies a Str';
    is-deeply "1+2i".conj, <1-2i>, 'to a Complex string too';
}

subtest 'Bridge', {
    plan 5;
    is-deeply 3.Bridge, 3e0, 'Int';
    is-deeply (1/4).Bridge, 0.25e0, 'Rat';
    is-deeply 2.5e0.Bridge, 2.5e0, 'Num';
    is-deeply 3.FatRat.Bridge, 3e0, 'FatRat';
    is-deeply (10**30).Bridge, 1e30, 'a big Int';
}

subtest 'lsb and msb', {
    plan 9;
    is 8.lsb, 3, 'lsb';
    is 12.lsb, 2, 'lsb of a mixed value';
    is-deeply 0.lsb, Nil, 'lsb of zero is Nil';
    is (2**70).lsb, 70, 'a big Int';
    is 8.msb, 3, 'msb';
    is (-8).msb, 3, 'msb of a negative';
    is (-1).msb, 0, 'msb of -1';
    is-deeply 0.msb, Nil, 'msb of zero is Nil';
    is True.lsb, 0, 'a Bool is an Int';
}

subtest 'chr', {
    plan 8;
    is 65.chr, 'A', 'Int';
    is "65".chr, 'A', 'Cool numifies a Str';
    is 65.5.chr, 'A', 'and truncates a Rat';
    is 0x1F600.chr.ord, 0x1F600, 'above the BMP';
    is 0x0F75.chr.codes, 2, 'a codepoint that decomposes is NFC-normalized';
    throws-like { (-1).chr }, X::AdHoc, message => /'out of bounds'/, 'a negative codepoint';
    throws-like { (10**30).chr }, X::AdHoc, message => /'out of bounds'/, 'a big codepoint';
    throws-like { 0x110000.chr }, X::AdHoc, message => /'out of bounds'/, 'past the last codepoint';
}

subtest 'the native integer coercions wrap', {
    plan 8;
    is 300.int8, 44, 'int8 wraps';
    is (-1).uint8, 255, 'uint8 wraps';
    is 70000.int16, 4464, 'int16';
    is 3.7.int, 3, 'int truncates a Rat';
    is "300".uint8, 44, 'Cool numifies a Str';
    is 5.byte, 5, 'byte';
    is (2**40).uint32, 0, 'uint32';
    is (-1).uint64, 18446744073709551615, 'uint64';
}

subtest 'rand', {
    plan 6;
    my @samples = (^50).map({ 10.rand });
    ok @samples.all ~~ Num && @samples.all >= 0 && @samples.all < 10, 'Int.rand is a Num in [0, 10)';
    ok (^50).map({ 2.5.rand }).all < 2.5, 'Rat.rand';
    ok (^50).map({ 0.5e0.rand }).all < 0.5, 'Num.rand';
    ok (^50).map({ "4".rand }).all < 4, 'Cool numifies a Str';
    ok (^50).map({ <a b c>.rand }).all < 3, 'a List numifies to its element count';
    ok (^50).map({ True.rand }).all < 1, 'a Bool is an Int';
}

subtest 'a method Cool does not declare is missing from a Str', {
    plan 4;
    throws-like { "5".lsb }, X::Method::NotFound, 'lsb';
    throws-like { "5".msb }, X::Method::NotFound, 'msb';
    throws-like { "5".Bridge }, X::Method::NotFound, 'Bridge';
    throws-like { [1, 2].narrow }, X::Method::NotFound, 'narrow of a List';
}

subtest 'the method table exposes the rows', {
    plan 7;
    ok Int.^can('lsb') && Int.^can('msb') && !Cool.^can('lsb'), 'lsb and msb belong to Int';
    ok Int.^can('narrow') && Rat.^can('narrow') && Complex.^can('narrow') && !Cool.^can('narrow'),
        'narrow belongs to the numeric types';
    ok Cool.^can('is-prime') && Cool.^can('conj') && Cool.^can('chr') && Cool.^can('rand'),
        'Cool declares is-prime, conj, chr and rand';
    ok Int.^can('Bridge') && Num.^can('Bridge') && Rat.^can('Bridge'), 'Bridge';
    ok Cool.^can('int8') && Int.^can('uint64') && Cool.^can('byte'), 'the native coercions';
    ok Bool.^can('lsb'), 'Bool inherits the Int rows';
    is-deeply (^3).map({ 65.chr }).List, <A A A>, 'a repeated call site answers each time';
}

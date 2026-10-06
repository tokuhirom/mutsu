use Test;

# ADR-11276 slice 3B: Uni's own methods (elems, codes, Int, Numeric, Str, list,
# gist, raku, AT-POS, EXISTS-POS) are handler rows. A Uni is a
# Positional[uint32] of codepoints; the normalization forms (NFC, NFD, NFKC,
# NFKD) are Unis. The shape is not Cool, so it reaches only the rows it owns.
# Expected values are Rakudo's.

plan 7;

my $uni = "abc".NFC;

subtest 'the codepoint count', {
    plan 4;
    is $uni.elems, 3, 'elems';
    is $uni.codes, 3, 'codes';
    is $uni.Int, 3, 'Int';
    is $uni.Numeric, 3, 'Numeric';
}

subtest 'a decomposed form counts its codepoints', {
    plan 3;
    my $nfd = "\x[E9]".NFD;
    is $nfd.elems, 2, 'NFD has two codepoints';
    is $nfd.NFC.elems, 1, 'NFC composes them';
    is $nfd.codes, 2, 'codes';
}

subtest 'Str composes to NFC', {
    plan 3;
    is "e\x[301]".NFD.Str, "\x[E9]", 'NFD.Str is the composed string';
    isa-ok $uni.Str, Str, 'a Str';
    is $uni.Str, 'abc', 'the characters';
}

subtest 'list answers the codepoints', {
    plan 3;
    is-deeply $uni.list.List, (97, 98, 99), 'the codepoints';
    is-deeply "\x[E9]".NFD.list.List, (101, 769), 'of a decomposition';
    is-deeply "".NFC.list.List, (), 'of nothing';
}

subtest 'gist and raku', {
    plan 4;
    is $uni.gist, 'NFC:0x<0061 0062 0063>', 'gist names the form';
    is "x".NFKD.gist, 'NFKD:0x<0078>', 'NFKD';
    is $uni.raku, 'Uni.new(0x0061, 0x0062, 0x0063).NFC', 'raku';
    is Uni.new(0x61, 0x62).gist, 'Uni:0x<0061 0062>', 'a plain Uni';
}

subtest 'AT-POS and EXISTS-POS', {
    plan 8;
    is $uni.AT-POS(0), 97, 'AT-POS';
    is $uni.AT-POS(2), 99, 'the last';
    is $uni.AT-POS("1"), 98, 'a Str index';
    ok $uni.AT-POS(9) ~~ Failure, 'past the end is a Failure';
    throws-like { $uni.AT-POS(-1).sink }, X::OutOfRange, 'a negative index';
    ok $uni.EXISTS-POS(2), 'EXISTS-POS of the last';
    nok $uni.EXISTS-POS(3), 'past the end';
    is $uni[1], 98, 'the subscript form';
}

subtest 'the method table exposes the rows', {
    plan 4;
    ok Uni.^can('elems') && Uni.^can('codes') && Uni.^can('list'), 'sizes and list';
    ok Uni.^can('gist') && Uni.^can('raku') && Uni.^can('Str'), 'rendering';
    ok Uni.^can('AT-POS') && Uni.^can('EXISTS-POS'), 'the positional subscript';
    is-deeply (^3).map({ "ab".NFC.elems }).List, (2, 2, 2), 'a repeated call site answers each time';
}

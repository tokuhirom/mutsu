use Test;

# ADR-11276 slice 3B: the Unicode methods of Cool, Str and Int (uniname,
# uninames, uniprop, uniprops, unival, univals, unimatch, uniparse,
# parse-names) and the normalization forms NFC, NFD, NFKC and NFKD (which Uni
# declares too) are handler rows. An Int reads its receiver as a codepoint;
# Str and Cool read the first character (all of them for the plural methods).
# Expected values are Rakudo's.

plan 12;

subtest 'uniname', {
    plan 6;
    is "a".uniname, 'LATIN SMALL LETTER A', 'the first character of a Str';
    is "abc".uniname, 'LATIN SMALL LETTER A', 'only the first';
    is 65.uniname, 'LATIN CAPITAL LETTER A', 'an Int is a codepoint';
    is-deeply "".uniname, Nil, 'the empty string is Nil';
    is 0x1F600.uniname, 'GRINNING FACE', 'above the BMP';
    is 1.5.uniname, 'DIGIT ONE', 'Cool stringifies another number';
}

subtest 'uninames', {
    plan 3;
    is-deeply "ab".uninames.List, ('LATIN SMALL LETTER A', 'LATIN SMALL LETTER B'), 'a name each';
    isa-ok "ab".uninames, Seq, 'a Seq';
    is-deeply "".uninames.List, (), 'empty';
}

subtest 'uniprop and uniprops', {
    plan 8;
    is "a".uniprop, 'Ll', 'the general category';
    is 65.uniprop, 'Lu', 'of a codepoint';
    is "a".uniprop("Script"), 'Latin', 'a named property';
    is 65.uniprop("Numeric_Value"), NaN, 'of a codepoint';
    is-deeply "aB".uniprops.List, ('Ll', 'Lu'), 'uniprops';
    isa-ok "aB".uniprops, Seq, 'is a Seq';
    is-deeply "aB".uniprops("Script").List, ('Latin', 'Latin'), 'uniprops with a property';
    is-deeply "".uniprop, Nil, 'the empty string is Nil';
}

subtest 'unival and univals', {
    plan 7;
    is "5".unival, 5, 'a digit';
    is "½".unival, 0.5, 'a fraction is a Rat';
    ok "x".unival.isNaN, 'a letter is NaN';
    is-deeply "a5½".univals.map(*.Str).List, ('NaN', '5', '0.5'), 'univals';
    isa-ok "a5".univals, Seq, 'a Seq';
    is 0x35.unival, 5, 'an Int is a codepoint';
    is-deeply "".unival, Nil, 'the empty string is Nil';
}

subtest 'unimatch', {
    plan 5;
    ok "a".unimatch("L"), 'a general category';
    nok "a".unimatch("Lu"), 'not another one';
    ok 65.unimatch("Lu"), 'an Int is a codepoint';
    ok "a".unimatch("Latin", "Script"), 'a value of a named property';
    nok "a".unimatch("Greek", "Script"), 'not another value';
}

subtest 'uniparse and parse-names', {
    plan 3;
    is "LATIN SMALL LETTER A".uniparse, 'a', 'one name';
    is "LATIN SMALL LETTER A,LATIN SMALL LETTER B".uniparse, 'ab', 'a comma-separated list';
    is "GRINNING FACE".uniparse.ord, 0x1F600, 'a name above the BMP';
}

subtest 'normalization forms', {
    plan 8;
    is "e\x[301]".NFC.list, (233,), 'NFC composes';
    is "\x[E9]".NFD.list, (101, 769), 'NFD decomposes';
    is "\x[FB01]".NFKC.Str, 'fi', 'NFKC applies the compatibility mapping';
    is "\x[FB01]".NFKD.list, (102, 105), 'NFKD too';
    isa-ok "abc".NFC, Uni, 'the answer is a Uni';
    is "abc".NFD.^name, 'NFD', 'named after its form';
    is-deeply "x".NFKC.list, (120,), 'ASCII is unchanged';
    is 42.NFC.list, (52, 50), 'Cool stringifies a number first';
}

subtest 'a Uni normalizes its own codepoints', {
    plan 4;
    my $nfd = "\x[E9]".NFD;
    is $nfd.NFC.list, (233,), 'NFD -> NFC';
    is $nfd.NFD.list, (101, 769), 'NFD -> NFD';
    is "\x[FB01]".NFC.NFKD.list, (102, 105), 'NFC -> NFKD';
    is $nfd.NFKC.^name, 'NFKC', 'the result is named after the form';
}

subtest 'Bool is an Int enum', {
    plan 3;
    is True.uniname, '<control-0001>', 'True.uniname is the name of codepoint 1';
    is False.uniprop, 'Cc', 'False.uniprop is the category of codepoint 0';
    ok True.unimatch("Cc"), 'True.unimatch';
}

subtest 'a type object cannot be asked', {
    plan 4;
    throws-like { Int.uniname }, X::Multi::NoMatch, 'Int.uniname';
    throws-like { Int.uniprop }, X::Multi::NoMatch, 'Int.uniprop';
    throws-like { Str.unival }, X::Multi::NoMatch, 'Str.unival';
    throws-like { Str.uniprop("Script") }, X::Multi::NoMatch, 'Str.uniprop with a property';
}

subtest 'the method table exposes the rows', {
    plan 6;
    ok Cool.^can('uniname') && Str.^can('uniname') && Int.^can('uniname'), 'uniname';
    ok Cool.^can('uniprops') && Str.^can('uniprops') && !Int.^can('uniprops') || Int.^can('uniprops'), 'uniprops';
    ok Cool.^can('unival') && Str.^can('univals') && Int.^can('unival'), 'unival and univals';
    ok Str.^can('unimatch') && Int.^can('unimatch'), 'unimatch';
    ok Cool.^can('NFC') && Str.^can('NFD') && Uni.^can('NFKC') && Uni.^can('NFKD'), 'the normalization forms';
    is-deeply (^3).map({ "a".uniname }).List, ('LATIN SMALL LETTER A' xx 3).List,
        'a repeated call site answers each time';
}

subtest 'Cool receivers other than Str and Int', {
    plan 3;
    is-deeply [1, 2].uniprops.List, ('Nd', 'Zs', 'Nd'), 'a List stringifies';
    is 1.5.uniprop, 'Nd', 'a Rat reads its first character';
    is-deeply 12.uniprops.List, ('Nd', 'Nd'), 'an Int reads its digits for the plural forms';
}

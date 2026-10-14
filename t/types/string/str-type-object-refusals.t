use Test;

plan 16;

# Type-object refusals of the `Str` methods (#12849), checked against Rakudo.
sub err-name(&code) { try code(); $!.^name }

# `Str:U` has no candidate for the normalization forms and `uniprops`.
is err-name({ Str.NFC }),  'X::Multi::NoMatch', 'Str.NFC';
is err-name({ Str.NFD }),  'X::Multi::NoMatch', 'Str.NFD';
is err-name({ Str.NFKC }), 'X::Multi::NoMatch', 'Str.NFKC';
is err-name({ Str.NFKD }), 'X::Multi::NoMatch', 'Str.NFKD';
is err-name({ Str.uniprops }), 'X::Multi::NoMatch', 'Str.uniprops';

# `encode` unboxes the invocant.
is err-name({ Str.encode }), 'X::AdHoc', 'Str.encode';
is err-name({ IntStr.encode }), 'X::AdHoc', 'IntStr.encode';

# An allomorph type object: `Str:D`-only methods fail the invocant binding,
# the `multi`s find no candidate.
is err-name({ IntStr.flip }), 'X::Parameter::InvalidConcreteness', 'IntStr.flip';
is err-name({ IntStr.uc }), 'X::Parameter::InvalidConcreteness', 'IntStr.uc';
is err-name({ NumStr.trim }), 'X::Parameter::InvalidConcreteness', 'NumStr.trim';
is err-name({ IntStr.NFC }), 'X::Multi::NoMatch', 'IntStr.NFC';
is err-name({ IntStr.contains("a") }), 'X::Multi::NoMatch', 'IntStr.contains';

# Defined receivers are unaffected.
is "a".NFC.list, (97,), 'Str:D.NFC';
is "ab".uniprops.elems, 2, 'Str:D.uniprops';
is "a".encode.list, (97,), 'Str:D.encode';
is <5>.flip, '5', 'an allomorph instance still answers';

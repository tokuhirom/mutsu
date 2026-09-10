use Test;

plan 30;

# `.uniname` for codepoints whose name is *derived* (`<prefix>-<hex>`) rather
# than listed in UnicodeData.txt, plus the `<surrogate-…>` / `<private-use-…>`
# sentinels. These used to come back as `<reserved-…>`: the implementation
# deferred to `unicode_names2`, whose name table covers whichever derived ranges
# its (older) UCD snapshot enumerated, and had no surrogate/private-use notion
# at all. A full 0..0x10FFFF diff against `raku` showed 145713 divergent
# codepoints in exactly four families; all four are pinned here at their range
# boundaries.

# Surrogates (D800..DFFF). Note these are not representable as a Raku
# character, so only the integer form is meaningful.
is 0xD800.uniname, '<surrogate-D800>', 'first surrogate';
is 0xDB7F.uniname, '<surrogate-DB7F>', 'last high surrogate';
is 0xDC00.uniname, '<surrogate-DC00>', 'first low surrogate';
is 0xDFFF.uniname, '<surrogate-DFFF>', 'last surrogate';
is 0xD7FF.uniname, '<reserved-D7FF>', 'just below the surrogate block is unaffected';

# Private Use Areas. The two supplementary planes stop at ..FFFD; their last
# two codepoints are noncharacters and keep that sentinel.
is 0xE000.uniname, '<private-use-E000>', 'BMP private use, first';
is 0xF8FF.uniname, '<private-use-F8FF>', 'BMP private use, last';
is 0xF0000.uniname, '<private-use-F0000>', 'plane 15 private use, first';
is 0xFFFFD.uniname, '<private-use-FFFFD>', 'plane 15 private use, last';
is 0x100000.uniname, '<private-use-100000>', 'plane 16 private use, first';
is 0x10FFFD.uniname, '<private-use-10FFFD>', 'plane 16 private use, last';
is 0xFFFFE.uniname, '<noncharacter-FFFFE>', 'noncharacter still wins over private use';
is 0xF8FE.uniname, '<private-use-F8FE>', 'inside the BMP private use area';

# Tangut Ideographs — absent from the name table entirely.
is 0x17000.uniname, 'TANGUT IDEOGRAPH-17000', 'Tangut, first';
is 0x187FF.uniname, 'TANGUT IDEOGRAPH-187FF', 'Tangut, last';
is 0x18D00.uniname, 'TANGUT IDEOGRAPH-18D00', 'Tangut Supplement, first';
is 0x18D1E.uniname, 'TANGUT IDEOGRAPH-18D1E', 'Tangut Supplement, last';
is 0x18D1F.uniname, '<reserved-18D1F>', 'just past Tangut Supplement is reserved';

# CJK Unified Ideographs — the name table stopped a few codepoints short of
# three of Rakudo's ranges.
is 0x2B81E.uniname, 'CJK UNIFIED IDEOGRAPH-2B81E', 'CJK range tail (Ext D/I)';
is 0x2EBEF.uniname, 'CJK UNIFIED IDEOGRAPH-2EBEF', 'CJK range tail (Ext J)';
is 0x3134F.uniname, 'CJK UNIFIED IDEOGRAPH-3134F', 'CJK range tail (Ext G)';
is 0x9FFF.uniname, 'CJK UNIFIED IDEOGRAPH-9FFF', 'CJK URO, last';
is 0x33479.uniname, 'CJK UNIFIED IDEOGRAPH-33479', 'CJK Ext H, last';
is 0x3347A.uniname, '<reserved-3347A>', 'just past the last CJK range is reserved';

# Families that already agreed must not regress.
is 0xAC00.uniname, 'HANGUL SYLLABLE GA', 'Hangul syllables keep their jamo name';
is 0x41.uniname, 'LATIN CAPITAL LETTER A', 'ordinary named character';

# `uniparse` is the inverse for the derived names — it used to reject the very
# names `.uniname` produces.
is uniparse('TANGUT IDEOGRAPH-17000').ord, 0x17000, 'uniparse resolves a Tangut name';
is uniparse('CJK UNIFIED IDEOGRAPH-2EBEF').ord, 0x2EBEF, 'uniparse resolves a CJK range tail';
is "\c[TANGUT IDEOGRAPH-18D00]".ord, 0x18D00, '\\c[...] resolves a derived name';
is uniparse('NOT A REAL NAME-1234').defined, False, 'a bogus derived-looking name is rejected';

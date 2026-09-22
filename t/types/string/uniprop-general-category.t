use Test;

# General_Category lookup, pinned across all 29 categories mutsu can answer
# and across the three tiers of the table that replaced the 28-regex linear
# probe (#8999): ASCII direct index, BMP two-stage trie, astral range search.
#
# Every expected value below was read off rakudo with `$cp.chr.uniprop`.

my @cases =
    # --- ASCII tier: cp < 0x80 ---
    (0x41,     'Lu', 'LATIN CAPITAL LETTER A'),
    (0x61,     'Ll', 'LATIN SMALL LETTER A'),
    (0x37,     'Nd', 'DIGIT SEVEN'),
    (0x20,     'Zs', 'SPACE'),
    (0x5F,     'Pc', 'LOW LINE'),
    (0x2D,     'Pd', 'HYPHEN-MINUS'),
    (0x28,     'Ps', 'LEFT PARENTHESIS'),
    (0x29,     'Pe', 'RIGHT PARENTHESIS'),
    (0x21,     'Po', 'EXCLAMATION MARK'),
    (0x2B,     'Sm', 'PLUS SIGN'),
    (0x24,     'Sc', 'DOLLAR SIGN'),
    (0x5E,     'Sk', 'CIRCUMFLEX ACCENT'),
    (0x0A,     'Cc', 'LINE FEED'),
    (0x09,     'Cc', 'TAB'),
    # --- BMP tier: 0x80 <= cp < 0x10000 ---
    (0x00AA,   'Lo', 'FEMININE ORDINAL INDICATOR'),
    (0x01C5,   'Lt', 'DZ WITH CARON, TITLECASE'),
    (0x02B0,   'Lm', 'MODIFIER LETTER SMALL H'),
    (0x0301,   'Mn', 'COMBINING ACUTE ACCENT'),
    (0x0903,   'Mc', 'DEVANAGARI SIGN VISARGA'),
    (0x0488,   'Me', 'COMBINING CYRILLIC HUNDRED THOUSANDS SIGN'),
    (0x16EE,   'Nl', 'RUNIC ARLAUG SYMBOL'),
    (0x00B2,   'No', 'SUPERSCRIPT TWO'),
    (0x203F,   'Pc', 'UNDERTIE'),
    (0x2010,   'Pd', 'HYPHEN'),
    (0x201C,   'Pi', 'LEFT DOUBLE QUOTATION MARK'),
    (0x201D,   'Pf', 'RIGHT DOUBLE QUOTATION MARK'),
    (0x00D7,   'Sm', 'MULTIPLICATION SIGN'),
    (0x00A3,   'Sc', 'POUND SIGN'),
    (0x00A0,   'Zs', 'NO-BREAK SPACE'),
    (0x2028,   'Zl', 'LINE SEPARATOR'),
    (0x2029,   'Zp', 'PARAGRAPH SEPARATOR'),
    (0x00AD,   'Cf', 'SOFT HYPHEN'),
    (0xE000,   'Co', 'PRIVATE USE FIRST'),
    (0x0378,   'Cn', 'UNASSIGNED'),
    (0x3042,   'Lo', 'HIRAGANA LETTER A'),
    (0x4E00,   'Lo', 'CJK UNIFIED IDEOGRAPH-4E00'),
    # --- astral tier: cp >= 0x10000 ---
    (0x1D400,  'Lu', 'MATHEMATICAL BOLD CAPITAL A'),
    (0x1D7CE,  'Nd', 'MATHEMATICAL BOLD DIGIT ZERO'),
    (0x12432,  'Nl', 'CUNEIFORM NUMERIC SIGN SHAR2 TIMES GAL PLUS DISH'),
    (0x1F600,  'So', 'GRINNING FACE'),
    (0x1F1E6,  'So', 'REGIONAL INDICATOR SYMBOL LETTER A'),
    (0x20000,  'Lo', 'CJK UNIFIED IDEOGRAPH-20000'),
    (0x10FFFF, 'Cn', 'the last codepoint'),
    ;

plan @cases.elems + 8;

for @cases -> ($cp, $want, $name) {
    is $cp.chr.uniprop, $want, "U+{$cp.base(16)} ($name) is $want";
}

# `.uniprop` and the explicit property name agree.
is 'A'.uniprop('General_Category'), 'Lu', "'General_Category' spelled out";
is 'A'.uniprop('gc'), 'Lu', "'gc' abbreviation";

# Parent-category and LC matching, which `unimatch` resolves through the same
# lookup.
ok 'A'.unimatch('L'),  'Lu matches the parent category L';
ok 'A'.unimatch('LC'), 'Lu matches the LC (cased letter) alias';
ok '_'.unimatch('P'),  'Pc matches the parent category P';
nok '_'.unimatch('L'), 'Pc does not match L';
nok 'ʰ'.unimatch('LC'), 'Lm is a letter but not a cased letter';
ok 'ʰ'.unimatch('L'),  'Lm matches the parent category L';

done-testing;

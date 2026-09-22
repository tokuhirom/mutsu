use Test;

# Script lookup, pinned across the three tiers of the table that replaced the
# 161-regex linear probe (#8999 follow-up): ASCII direct index, BMP two-stage
# trie, astral range search.
#
# Every expected value below was read off rakudo with `$cp.chr.uniprop('Script')`.

my @cases =
    # --- ASCII tier: cp < 0x80. Note digits, space and punctuation are
    # Common, NOT Latin -- only the letters are Latin.
    (0x41,     'Latin',     'LATIN CAPITAL LETTER A'),
    (0x7A,     'Latin',     'LATIN SMALL LETTER Z'),
    (0x37,     'Common',    'DIGIT SEVEN'),
    (0x20,     'Common',    'SPACE'),
    (0x2B,     'Common',    'PLUS SIGN'),
    (0x5F,     'Common',    'LOW LINE'),
    (0x0A,     'Common',    'LINE FEED'),
    # --- BMP tier: 0x80 <= cp < 0x10000 ---
    (0x00C0,   'Latin',     'LATIN CAPITAL A WITH GRAVE'),
    (0x0391,   'Greek',     'GREEK CAPITAL ALPHA'),
    (0x0410,   'Cyrillic',  'CYRILLIC CAPITAL A'),
    (0x0531,   'Armenian',  'ARMENIAN CAPITAL AYB'),
    (0x05D0,   'Hebrew',    'HEBREW LETTER ALEF'),
    (0x0627,   'Arabic',    'ARABIC LETTER ALEF'),
    (0x0E01,   'Thai',      'THAI CHARACTER KO KAI'),
    (0x16A0,   'Runic',     'RUNIC LETTER FEHU'),
    (0x3042,   'Hiragana',  'HIRAGANA LETTER A'),
    (0x30A2,   'Katakana',  'KATAKANA LETTER A'),
    (0x4E00,   'Han',       'CJK UNIFIED IDEOGRAPH-4E00'),
    (0xAC00,   'Hangul',    'HANGUL SYLLABLE GA'),
    (0x0301,   'Inherited', 'COMBINING ACUTE ACCENT'),
    (0x0378,   'Unknown',   'unassigned'),
    (0xE000,   'Unknown',   'private use'),
    # --- astral tier: cp >= 0x10000 ---
    (0x12000,  'Cuneiform', 'CUNEIFORM SIGN A'),
    (0x1E900,  'Adlam',     'ADLAM CAPITAL ALIF'),
    (0x20000,  'Han',       'CJK UNIFIED IDEOGRAPH-20000'),
    (0x1D400,  'Common',    'MATHEMATICAL BOLD CAPITAL A'),
    (0x1F600,  'Common',    'GRINNING FACE'),
    (0x10FFFF, 'Unknown',   'the last codepoint'),
    ;

plan @cases.elems + 6;

for @cases -> ($cp, $want, $name) {
    is $cp.chr.uniprop('Script'), $want, "U+{$cp.base(16)} ($name) is $want";
}

# The `sc` abbreviation resolves to the same property.
is 'A'.uniprop('sc'), 'Latin', "'sc' abbreviation";

# `unimatch` against a script name, which resolves through the same lookup.
ok 'あ'.unimatch('Hiragana', 'Script'), 'hiragana matches Script=Hiragana';
nok 'あ'.unimatch('Katakana', 'Script'), 'hiragana does not match Script=Katakana';
ok '一'.unimatch('Han', 'Script'), 'CJK ideograph matches Script=Han';

# The regex Script assertion reaches the same table.
ok 'あ' ~~ /<:Script<Hiragana>>/, '<:Script<Hiragana>> matches hiragana';
nok 'a' ~~ /<:Script<Hiragana>>/, '<:Script<Hiragana>> does not match a latin letter';

done-testing;

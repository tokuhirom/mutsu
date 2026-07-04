use Test;

# Line_Break property for ideographic-script letters must be ID (Ideographic),
# not the generic AL (Alphabetic) fallback used for other letters.

plan 12;

# Han (CJK ideographs)
is '漢'.uniprop('Line_Break'), 'ID', "Han ideograph is Line_Break=ID";
is '字'.uniprop('Line_Break'), 'ID', "another Han ideograph is ID";

# Hiragana / Katakana
is 'あ'.uniprop('Line_Break'), 'ID', "Hiragana is ID";
is 'ア'.uniprop('Line_Break'), 'ID', "Katakana is ID";

# Bopomofo, Yi
is 'ㄅ'.uniprop('Line_Break'), 'ID', "Bopomofo is ID";
is 'ꀀ'.uniprop('Line_Break'), 'ID', "Yi syllable is ID";

# Codepoint form works too
is 0x6F22.uniprop('Line_Break'), 'ID', "0x6F22 (漢) via codepoint is ID";
is 0x3042.uniprop('Line_Break'), 'ID', "0x3042 (あ) via codepoint is ID";

# Non-ideographic letters remain AL
is 'A'.uniprop('Line_Break'), 'AL', "Latin letter stays AL";
is 'α'.uniprop('Line_Break'), 'AL', "Greek letter stays AL";

# Existing behaviour still holds
is "\n".uniprop('Line_Break'), 'LF', 'newline is LF';
is 0x200D.uniprop('Line_Break'), 'ZWJ', 'ZWJ is ZWJ';

use Test;

# Word_Break property (UAX #29). mutsu previously classified every letter as
# ALetter and every non-letter as Other; verify the refined classification
# matches Rakudo / the Unicode spec.

plan 30;

# ALetter: alphabetic, non-ideographic, non-Hiragana, non-Complex_Context
is 'A'.uniprop('Word_Break'), 'ALetter', 'Latin letter is ALetter';
is 'é'.uniprop('Word_Break'), 'ALetter', 'accented Latin is ALetter';
is 'ㄅ'.uniprop('Word_Break'), 'ALetter', 'Bopomofo is ALetter';
is 'ꀀ'.uniprop('Word_Break'), 'ALetter', 'Yi is ALetter';
is '한'.uniprop('Word_Break'), 'ALetter', 'Hangul syllable is ALetter';
is "\x3005".uniprop('Word_Break'), 'ALetter', 'ideographic iteration mark (non-Ideographic) is ALetter';

# Ideographs / Hiragana / Complex_Context -> Other (not ALetter)
is '漢'.uniprop('Word_Break'), 'Other', 'Han ideograph is Other';
is 'あ'.uniprop('Word_Break'), 'Other', 'Hiragana is Other';
is 'ก'.uniprop('Word_Break'), 'Other', 'Thai letter (Complex_Context) is Other';

# Katakana
is 'ア'.uniprop('Word_Break'), 'Katakana', 'Katakana letter is Katakana';
is "\x30FC".uniprop('Word_Break'), 'Katakana', 'prolonged sound mark is Katakana';
is "\xFF70".uniprop('Word_Break'), 'Katakana', 'halfwidth prolonged mark is Katakana';

# Numeric
is '9'.uniprop('Word_Break'), 'Numeric', 'digit is Numeric';

# Hebrew_Letter preserved
is "\xFB1F".uniprop('Word_Break'), 'Hebrew_Letter', 'Hebrew letter is Hebrew_Letter';

# ExtendNumLet (Connector_Punctuation)
is '_'.uniprop('Word_Break'), 'ExtendNumLet', 'low line is ExtendNumLet';
is "\x203F".uniprop('Word_Break'), 'ExtendNumLet', 'undertie is ExtendNumLet';

# Punctuation classes
is "'".uniprop('Word_Break'), 'Single_Quote', 'apostrophe is Single_Quote';
is '"'.uniprop('Word_Break'), 'Double_Quote', 'quotation mark is Double_Quote';
is '.'.uniprop('Word_Break'), 'MidNumLet', 'full stop is MidNumLet';
is "\x2018".uniprop('Word_Break'), 'MidNumLet', 'left single quote is MidNumLet';
is ':'.uniprop('Word_Break'), 'MidLetter', 'colon is MidLetter';
is "\x00B7".uniprop('Word_Break'), 'MidLetter', 'middle dot is MidLetter';
is ','.uniprop('Word_Break'), 'MidNum', 'comma is MidNum';
is ';'.uniprop('Word_Break'), 'MidNum', 'semicolon is MidNum';

# Line separators
is "\n".uniprop('Word_Break'), 'LF', 'LF is LF';
is "\r".uniprop('Word_Break'), 'CR', 'CR is CR';
is "\x2028".uniprop('Word_Break'), 'Newline', 'LINE SEPARATOR is Newline';

# Extend / ZWJ / Format
is "\x0301".uniprop('Word_Break'), 'Extend', 'combining mark is Extend';
is "\x200D".uniprop('Word_Break'), 'ZWJ', 'ZWJ is ZWJ';
is "\xAD".uniprop('Word_Break'), 'Format', 'soft hyphen is Format';

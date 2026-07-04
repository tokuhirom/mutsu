use Test;

# Sentence_Break property (UAX #29). mutsu previously only produced
# CR/LF/Sp/Upper/Lower/Numeric/ATerm/STerm/Other; verify the full set of
# classes against Rakudo / the Unicode spec.

plan 28;

# CR / LF / Sep
is "\r".uniprop('Sentence_Break'), 'CR', 'CR is CR';
is "\n".uniprop('Sentence_Break'), 'LF', 'LF is LF';
is "\x0085".uniprop('Sentence_Break'), 'Sep', 'NEL is Sep';
is "\x2028".uniprop('Sentence_Break'), 'Sep', 'LINE SEPARATOR is Sep';
is "\x2029".uniprop('Sentence_Break'), 'Sep', 'PARAGRAPH SEPARATOR is Sep';

# Sp
is "\t".uniprop('Sentence_Break'), 'Sp', 'TAB is Sp';
is ' '.uniprop('Sentence_Break'), 'Sp', 'SPACE is Sp';
is "\xA0".uniprop('Sentence_Break'), 'Sp', 'NBSP is Sp';

# Extend (marks + ZWJ)
is "\x0301".uniprop('Sentence_Break'), 'Extend', 'combining mark is Extend';
is "\x0903".uniprop('Sentence_Break'), 'Extend', 'spacing mark (Mc) is Extend';
is "\x200D".uniprop('Sentence_Break'), 'Extend', 'ZWJ is Extend';

# Format
is "\xAD".uniprop('Sentence_Break'), 'Format', 'soft hyphen is Format';
is "\x0600".uniprop('Sentence_Break'), 'Format', 'ARABIC NUMBER SIGN is Format';

# Numeric / Upper / Lower / OLetter
is '5'.uniprop('Sentence_Break'), 'Numeric', 'digit is Numeric';
is 'A'.uniprop('Sentence_Break'), 'Upper', 'A is Upper';
is "\x01C5".uniprop('Sentence_Break'), 'Upper', 'titlecase letter is Upper';
is 'a'.uniprop('Sentence_Break'), 'Lower', 'a is Lower';
is "\xAA".uniprop('Sentence_Break'), 'Lower', 'feminine ordinal is Lower';
is "\x05D0".uniprop('Sentence_Break'), 'OLetter', 'Hebrew alef is OLetter';
is "\x3042".uniprop('Sentence_Break'), 'OLetter', 'Hiragana is OLetter';

# ATerm / STerm
is '.'.uniprop('Sentence_Break'), 'ATerm', 'full stop is ATerm';
is "\xFF0E".uniprop('Sentence_Break'), 'ATerm', 'fullwidth full stop is ATerm';
is '!'.uniprop('Sentence_Break'), 'STerm', 'exclamation is STerm';
is '?'.uniprop('Sentence_Break'), 'STerm', 'question mark is STerm';

# Close
is '('.uniprop('Sentence_Break'), 'Close', 'open paren is Close';
is '"'.uniprop('Sentence_Break'), 'Close', 'quote is Close';

# SContinue
is ','.uniprop('Sentence_Break'), 'SContinue', 'comma is SContinue';
is ':'.uniprop('Sentence_Break'), 'SContinue', 'colon is SContinue';

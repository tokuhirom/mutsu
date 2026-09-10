use Test;

# Bidi_Class of a letter depends on its script: Arabic-family scripts are
# Arabic_Letter (AL) and other right-to-left scripts are Right_to_Left (R).
# mutsu previously classified every letter as Left_to_Right (L).

plan 22;

# Arabic-family letters -> AL
is 'ب'.uniprop('Bidi_Class'), 'AL', 'Arabic beh is AL';
is 'ا'.uniprop('Bidi_Class'), 'AL', 'Arabic alef is AL';
is "\x0710".uniprop('Bidi_Class'), 'AL', 'Syriac alaph is AL';
is "\x0780".uniprop('Bidi_Class'), 'AL', 'Thaana letter is AL';
is "\x0640".uniprop('Bidi_Class'), 'AL', 'Arabic tatweel (Lm) is AL';

# Hebrew and other RTL scripts -> R
is 'א'.uniprop('Bidi_Class'), 'R', 'Hebrew alef is R';
is "\x0800".uniprop('Bidi_Class'), 'R', 'Samaritan letter is R';
is "\x07CA".uniprop('Bidi_Class'), 'R', 'NKo letter is R';
is "\x10800".uniprop('Bidi_Class'), 'R', 'Cypriot syllable is R';
is "\x10900".uniprop('Bidi_Class'), 'R', 'Phoenician letter is R';
is "\x1E900".uniprop('Bidi_Class'), 'R', 'Adlam letter is R';

# LTR scripts stay L
is 'A'.uniprop('Bidi_Class'), 'L', 'Latin is L';
is 'Ω'.uniprop('Bidi_Class'), 'L', 'Greek is L';
is 'あ'.uniprop('Bidi_Class'), 'L', 'Hiragana is L';
is '漢'.uniprop('Bidi_Class'), 'L', 'Han is L';
is 'क'.uniprop('Bidi_Class'), 'L', 'Devanagari is L';

# Numbers and marks are unaffected by the letter rule
is '5'.uniprop('Bidi_Class'), 'EN', 'ASCII digit is EN';
is "\x0661".uniprop('Bidi_Class'), 'AN', 'Arabic-Indic digit is AN';
is 'Ⅴ'.uniprop('Bidi_Class'), 'L', 'Roman numeral (Nl) is L';
is "\x05B0".uniprop('Bidi_Class'), 'NSM', 'Hebrew point is NSM';
is "\x064B".uniprop('Bidi_Class'), 'NSM', 'Arabic fathatan is NSM';
is ' '.uniprop('Bidi_Class'), 'WS', 'space is WS';

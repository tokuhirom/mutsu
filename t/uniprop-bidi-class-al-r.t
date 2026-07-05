use Test;

# Bidi_Class AL (Arabic_Letter) and R (Right_to_Left) are per-codepoint
# classes covering the letters, marks, digits, punctuation and symbols of the
# relevant scripts. mutsu previously used a script heuristic that only handled
# letters, leaving RTL-script digits, symbols and marks as L.

plan 18;

# Arabic_Letter (AL): letters, marks, digits stay/become AL
is "\x0627".uniprop('Bidi_Class'), 'AL', 'Arabic alef is AL';
is "\x0710".uniprop('Bidi_Class'), 'AL', 'Syriac alaph is AL';
is "\x0660".uniprop('Bidi_Class'), 'AN', 'Arabic-Indic digit is AN (not AL)';
is "\x0608".uniprop('Bidi_Class'), 'AL', 'Arabic ray (Sm) is AL';
is "\x060B".uniprop('Bidi_Class'), 'AL', 'Afghani sign (Sc) is AL';
is "\x06FD".uniprop('Bidi_Class'), 'AL', 'Arabic sign sindhi ampersand (So) is AL';
is "\x064B".uniprop('Bidi_Class'), 'AL', 'Arabic fathatan (mark) is AL';
is "\x1EE00".uniprop('Bidi_Class'), 'AL', 'Arabic math alef is AL';

# Right_to_Left (R): Hebrew and other RTL scripts
is "\x05D0".uniprop('Bidi_Class'), 'R', 'Hebrew alef is R';
is "\x05BE".uniprop('Bidi_Class'), 'R', 'Hebrew maqaf is R';
is "\x07C0".uniprop('Bidi_Class'), 'R', 'NKo digit zero is R';
is "\x07CA".uniprop('Bidi_Class'), 'R', 'NKo letter is R';
is "\x10800".uniprop('Bidi_Class'), 'R', 'Cypriot syllable is R';
is "\x1E900".uniprop('Bidi_Class'), 'R', 'Adlam letter is R';

# LTR content stays L / other classes unchanged
is 'A'.uniprop('Bidi_Class'), 'L', 'Latin letter is L';
is '5'.uniprop('Bidi_Class'), 'EN', 'digit is EN';
is '!'.uniprop('Bidi_Class'), 'ON', 'exclamation is ON';
is "\x0301".uniprop('Bidi_Class'), 'NSM', 'combining mark is NSM';

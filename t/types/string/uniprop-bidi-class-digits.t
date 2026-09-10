use Test;

plan 12;

# Bidi_Class of a numeric character depends on its specific digit set, not just
# its General_Category: European-style digits (ASCII, fullwidth, superscript,
# subscript, extended Arabic-Indic) are EN; Arabic-Indic digits are AN. A plain
# General_Category heuristic wrongly reports all of them as L.

is '0'.uniprop('Bidi_Class'), 'EN', 'ASCII digit 0 is EN';
is '9'.uniprop('Bidi_Class'), 'EN', 'ASCII digit 9 is EN';
is 0xFF10.uniprop('Bidi_Class'), 'EN', 'fullwidth digit is EN';
is 0x00B2.uniprop('Bidi_Class'), 'EN', 'superscript two is EN';
is 0x2070.uniprop('Bidi_Class'), 'EN', 'superscript zero is EN';
is 0x2080.uniprop('Bidi_Class'), 'EN', 'subscript zero is EN';
is 0x06F0.uniprop('Bidi_Class'), 'EN', 'extended Arabic-Indic digit is EN';

is 0x0660.uniprop('Bidi_Class'), 'AN', 'Arabic-Indic digit zero is AN';
is 0x0669.uniprop('Bidi_Class'), 'AN', 'Arabic-Indic digit nine is AN';

# Digits of other scripts remain L, and letters/controls are unaffected.
is '৫'.uniprop('Bidi_Class'), 'L', 'a Bengali digit stays L';
is 'A'.uniprop('Bidi_Class'), 'L', 'a Latin letter is L';
is 0x202A.uniprop('Bidi_Class'), 'LRE', 'the LEFT-TO-RIGHT EMBEDDING control is LRE';

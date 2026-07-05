use Test;

# Bidi_Class=EN (European_Number) covers more than the ASCII/fullwidth
# digits: digit-full-stop forms, Coptic epact numbers, mathematical digits
# and the dingbat/segmented digit forms. mutsu previously left these as L.

plan 12;

is '5'.uniprop('Bidi_Class'), 'EN', 'ASCII digit is EN';
is '²'.uniprop('Bidi_Class'), 'EN', 'superscript two is EN';
is "\xFF15".uniprop('Bidi_Class'), 'EN', 'fullwidth five is EN';
is "\x06F5".uniprop('Bidi_Class'), 'EN', 'extended Arabic-Indic five is EN';

# Newly covered
is "\x2488".uniprop('Bidi_Class'), 'EN', 'digit one full stop is EN';
is "\x102E1".uniprop('Bidi_Class'), 'EN', 'Coptic epact digit one is EN';
is "\x1D7CE".uniprop('Bidi_Class'), 'EN', 'math bold digit zero is EN';
is "\x1F101".uniprop('Bidi_Class'), 'EN', 'digit zero comma is EN';
is "\x1FBF5".uniprop('Bidi_Class'), 'EN', 'segmented digit five is EN';

# Arabic-Indic digits stay AN, not EN
is "\x0661".uniprop('Bidi_Class'), 'AN', 'Arabic-Indic one is AN';
# Non-digit stays L / ON
is 'A'.uniprop('Bidi_Class'), 'L', 'letter is L';
is '!'.uniprop('Bidi_Class'), 'ON', 'exclamation is ON';

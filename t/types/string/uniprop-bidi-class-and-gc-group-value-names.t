use v6;
use Test;

# `uniprop` reads a bare property VALUE name as the property it belongs to:
# a Bidi_Class value answers the Bidi_Class, a one-letter General_Category
# group answers that binary property (#16491).

plan 12;

is 'a'.uniprop('BN'), 'L', 'Bidi_Class of a Latin letter';
is 'א'.uniprop('R'), 'R', 'Bidi_Class of a Hebrew letter';
is ' '.uniprop('WS'), 'WS', 'Bidi_Class of a space';
is '1'.uniprop('ON'), 'EN', 'Bidi_Class of a digit';
is "\x[202A]".uniprop('LRE'), 'LRE', 'Bidi_Class of an embedding mark';
is 'ا'.uniprop('AL'), 'AL', 'Bidi_Class of an Arabic letter';

is 'a'.uniprop('L'), 1, 'L is the Letter group';
is '1'.uniprop('L'), 0, 'a digit is not a Letter';
is '+'.uniprop('S'), 1, 'S is the Symbol group';
is 'a'.uniprop('S'), 0, 'a letter is not a Symbol';
is '1'.uniprop('N'), 1, 'N is the Number group';
is "\x[0301]".uniprop('M'), 1, 'M is the Mark group';

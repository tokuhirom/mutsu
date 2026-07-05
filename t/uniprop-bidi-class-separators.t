use Test;

# Bidi_Class of separators, terminators, format controls and right-to-left
# script punctuation. mutsu previously returned L / NSM for most of these.

plan 26;

# European_Separator (ES): plus / minus
is '+'.uniprop('Bidi_Class'), 'ES', 'plus is ES';
is '-'.uniprop('Bidi_Class'), 'ES', 'hyphen-minus is ES';
is "\x2212".uniprop('Bidi_Class'), 'ES', 'minus sign is ES';

# Common_Separator (CS): comma, dot, slash, colon, NBSP
is ','.uniprop('Bidi_Class'), 'CS', 'comma is CS';
is '.'.uniprop('Bidi_Class'), 'CS', 'full stop is CS';
is '/'.uniprop('Bidi_Class'), 'CS', 'solidus is CS';
is ':'.uniprop('Bidi_Class'), 'CS', 'colon is CS';
is "\xA0".uniprop('Bidi_Class'), 'CS', 'NBSP is CS';

# European_Terminator (ET): currency, percent, degree
is '$'.uniprop('Bidi_Class'), 'ET', 'dollar is ET';
is '%'.uniprop('Bidi_Class'), 'ET', 'percent is ET';
is '#'.uniprop('Bidi_Class'), 'ET', 'number sign is ET';
is "\xB0".uniprop('Bidi_Class'), 'ET', 'degree sign is ET';
is "\x20AC".uniprop('Bidi_Class'), 'ET', 'euro sign is ET';

# Segment_Separator (S) and Whitespace
is "\t".uniprop('Bidi_Class'), 'S', 'tab is S';
is "\x000B".uniprop('Bidi_Class'), 'S', 'vertical tab is S';
is "\x000C".uniprop('Bidi_Class'), 'WS', 'form feed is WS';
is "\x2028".uniprop('Bidi_Class'), 'WS', 'line separator is WS';
is "\x2029".uniprop('Bidi_Class'), 'B', 'paragraph separator is B';

# Boundary_Neutral (BN): format controls
is "\xAD".uniprop('Bidi_Class'), 'BN', 'soft hyphen is BN';
is "\x200B".uniprop('Bidi_Class'), 'BN', 'ZWSP is BN';
is "\x061C".uniprop('Bidi_Class'), 'AL', 'Arabic letter mark is AL';

# Arabic_Number (AN): Rumi numeral symbols
is "\x10E60".uniprop('Bidi_Class'), 'AN', 'Rumi digit one is AN';

# Right-to-left script punctuation inherits the script direction
is "\x061F".uniprop('Bidi_Class'), 'AL', 'Arabic question mark is AL';
is "\x060C".uniprop('Bidi_Class'), 'CS', 'Arabic comma is CS';
is "\x05C3".uniprop('Bidi_Class'), 'R', 'Hebrew sof pasuq is R';
is "\x05BE".uniprop('Bidi_Class'), 'R', 'Hebrew maqaf is R';

use Test;

# Bidi_Class=ON (Other_Neutral) for neutral punctuation, math operators and
# symbols. mutsu previously left these as L (the General_Category default).

plan 20;

# ASCII neutral punctuation / symbols
is '!'.uniprop('Bidi_Class'), 'ON', 'exclamation is ON';
is '('.uniprop('Bidi_Class'), 'ON', 'open paren is ON';
is ')'.uniprop('Bidi_Class'), 'ON', 'close paren is ON';
is '&'.uniprop('Bidi_Class'), 'ON', 'ampersand is ON';
is '?'.uniprop('Bidi_Class'), 'ON', 'question mark is ON';
is '='.uniprop('Bidi_Class'), 'ON', 'equals is ON';
is '['.uniprop('Bidi_Class'), 'ON', 'open bracket is ON';

# Latin-1 symbols
is "\xAB".uniprop('Bidi_Class'), 'ON', 'left guillemet is ON';
is "\xA9".uniprop('Bidi_Class'), 'ON', 'copyright sign is ON';
is "\xD7".uniprop('Bidi_Class'), 'ON', 'multiplication sign is ON';

# Math / arrows / symbols
is "\x2010".uniprop('Bidi_Class'), 'ON', 'hyphen is ON';
is "\x2200".uniprop('Bidi_Class'), 'ON', 'for all is ON';
is "\x2190".uniprop('Bidi_Class'), 'ON', 'leftwards arrow is ON';
is "\x2600".uniprop('Bidi_Class'), 'ON', 'black sun symbol is ON';
is "\x1F600".uniprop('Bidi_Class'), 'ON', 'grinning face emoji is ON';
is "\x3001".uniprop('Bidi_Class'), 'ON', 'ideographic comma is ON';

# These specific classes must NOT be swallowed by ON
is '+'.uniprop('Bidi_Class'), 'ES', 'plus stays ES';
is ','.uniprop('Bidi_Class'), 'CS', 'comma stays CS';
is '$'.uniprop('Bidi_Class'), 'ET', 'dollar stays ET';
is 'A'.uniprop('Bidi_Class'), 'L', 'letter stays L';

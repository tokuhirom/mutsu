use Test;
use nqp;

# `nqp::iscclass` answers every `CCLASS_*` bit from one General_Category table
# load plus a handful of codepoint tests (#8999), instead of thirteen string
# comparisons against the category name. This pins the whole membership matrix
# it derives, so a wrong bit in that table cannot pass unnoticed.
#
# Every expected row was read off rakudo.

my @classes =
    ('UPPERCASE',    1),
    ('LOWERCASE',    2),
    ('ALPHABETIC',   4),
    ('NUMERIC',      8),
    ('HEXADECIMAL',  16),
    ('WHITESPACE',   32),
    ('PRINTING',     64),
    ('BLANK',        256),
    ('CONTROL',      512),
    ('PUNCTUATION',  1024),
    ('ALPHANUMERIC', 2048),
    ('NEWLINE',      4096),
    ('WORD',         8192),
    ;

my @cases =
    ('A',      'UPPERCASE,ALPHABETIC,HEXADECIMAL,PRINTING,ALPHANUMERIC,WORD'),
    ('z',      'LOWERCASE,ALPHABETIC,PRINTING,ALPHANUMERIC,WORD'),
    ('f',      'LOWERCASE,ALPHABETIC,HEXADECIMAL,PRINTING,ALPHANUMERIC,WORD'),
    ('5',      'NUMERIC,HEXADECIMAL,PRINTING,ALPHANUMERIC,WORD'),
    (' ',      'WHITESPACE,PRINTING,BLANK'),
    ("\t",     'WHITESPACE,BLANK,CONTROL'),
    ("\n",     'WHITESPACE,CONTROL,NEWLINE'),
    ("\x[0C]", 'WHITESPACE,CONTROL,NEWLINE'),
    ("\x[85]", 'WHITESPACE,CONTROL,NEWLINE'),
    ("\x[A0]", 'WHITESPACE,PRINTING,BLANK'),
    ("\x[2028]", 'WHITESPACE,PRINTING,NEWLINE'),
    ("\x[2029]", 'WHITESPACE,PRINTING,NEWLINE'),
    ("\x[00]", 'CONTROL'),
    ('_',      'PRINTING,PUNCTUATION,WORD'),
    ('!',      'PRINTING,PUNCTUATION'),
    ('-',      'PRINTING,PUNCTUATION'),
    ('+',      'PRINTING'),
    # CCLASS_ALPHABETIC is L*, not the Unicode Alphabetic property: U+2160
    # ROMAN NUMERAL ONE is Nl and Alphabetic=Yes, and rakudo answers 0.
    ("\x[2160]", 'PRINTING'),
    ("\x[B2]", 'PRINTING'),          # No, not NUMERIC either
    ("\x[301]", 'PRINTING'),         # Mn
    ("\x[AD]", 'PRINTING'),          # Cf is printing; only Cc is not
    ("\x[3042]", 'ALPHABETIC,PRINTING,ALPHANUMERIC,WORD'),
    ("\x[1D400]", 'UPPERCASE,ALPHABETIC,PRINTING,ALPHANUMERIC,WORD'),
    ("\x[1F600]", 'PRINTING'),
    ;

plan @cases.elems + 5;

for @cases -> ($ch, $want) {
    my @hit;
    for @classes -> ($name, $bit) {
        @hit.push($name) if nqp::iscclass($bit, $ch, 0);
    }
    is @hit.join(','), $want, "U+{$ch.ord.base(16)} cclass membership";
}

# The scanning ops read the same bits.
my str $text = "abc 123";
is nqp::findnotcclass(nqp::const::CCLASS_WORD, $text, 0, nqp::chars($text)), 3,
    'findnotcclass stops at the space';
is nqp::findcclass(nqp::const::CCLASS_WHITESPACE, $text, 0, nqp::chars($text)), 3,
    'findcclass finds the space';
is nqp::findnotcclass(nqp::const::CCLASS_NUMERIC, '123x', 0, 4), 3,
    'findnotcclass stops at the non-digit';

# CCLASS_ANY (65535) matches everything, including a control character.
is nqp::iscclass(65535, "\x[00]", 0), 1, 'CCLASS_ANY matches NUL';
is nqp::iscclass(65535, "\x[1F600]", 0), 1, 'CCLASS_ANY matches an astral char';

done-testing;

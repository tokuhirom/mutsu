use v6;
use Test;

plan 9;

# `utf8` is the one Blob type whose `.Str` works: it decodes. Every other
# Blob/Buf flavour throws X::Buf::AsStr.

is "bumble".encode.Str, "bumble", 'utf8.Str decodes';
is utf8.new(98, 117).Stringy, "bu", 'utf8.Stringy decodes';
is "sn\c[SNOWMAN]w".encode.Str, "sn\c[SNOWMAN]w", 'multi-byte codepoints round-trip';

dies-ok { Buf.new(98, 117).Str }, 'Buf.Str dies';
dies-ok { Blob.new(98, 117).Str }, 'Blob.Str dies';
dies-ok { "x".encode('latin-1').Str }, 'Blob[uint8].Str dies';

# `eq` goes through `.Str`, so a utf8 compares as its decoded text.
ok "bumble".encode eq "bumble", 'utf8 eq Str compares decoded';
nok "bumble".encode eq "x", 'and is False when they differ';

# `is` uses the same coercion.
is "bumble".encode, "bumble", 'is() compares a utf8 against a Str';

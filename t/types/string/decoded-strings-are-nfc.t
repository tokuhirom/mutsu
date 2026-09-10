use Test;

plan 8;

# Raku's Str is NFG, so a string built from bytes is normalized at creation.
# mutsu normalized string *literals* at parse time but not `.decode` output, so
# a decoded string compared unequal to an identical literal even though `.ords`
# — which normalizes on read — reported the same code points.

{
    # U+2126 OHM SIGN composes to U+03A9 GREEK CAPITAL LETTER OMEGA.
    my $s = Buf.new(0xE2, 0x84, 0xA6).decode('utf-8');
    is $s, "\x[03A9]", 'a decoded OHM SIGN equals a literal OMEGA';
    is $s.encode('utf-8').elems, 2, 'and is stored composed';
}

{
    # "e" + COMBINING ACUTE composes to U+00E9.
    my $s = Buf.new(0x65, 0xCC, 0x81).decode('utf-8');
    is $s, "\x[00E9]", 'a decoded decomposed e-acute equals the composed literal';
    is $s.chars, 1, 'and is one character';
}

{
    # latin-1 has no combining marks, so decoding is unaffected.
    my $s = Buf.new(0xE1, 0xE2).decode('latin-1');
    is $s.chars, 2, 'a latin-1 decode is unchanged';
    is $s, "\x[00E1]\x[00E2]", 'and holds the right characters';
}

{
    # utf8-c8 must NOT be normalized: keeping invalid bytes as synthetics is the
    # whole point of the encoding, and a round-trip has to be exact.
    my $b = Buf.new(0xC3, 0x28);
    is $b.decode('utf8-c8').encode('utf8-c8').list, (0xC3, 0x28),
        'utf8-c8 still round-trips invalid bytes';
}

{
    # A decoded string is usable as a hash key against a literal (this is what
    # broke Cro's percent-decoded query keys).
    my %h;
    %h{Buf.new(0xE2, 0x84, 0xA6).decode('utf-8')} = 'v';
    is %h{"\x[03A9]"}, 'v', 'a decoded string works as a hash key';
}

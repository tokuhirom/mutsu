use Test;

plan 8;

# The streaming `Encoding::Decoder` used to decode every buffer as UTF-8 no
# matter which encoding it was built with, so a latin-1 decoder turned every
# high byte into U+FFFD. Cro builds its HTTP header decoder exactly that way
# (`Encoding::Registry.find('iso-8859-1').decoder()`), which mangled any header
# value outside ASCII.

my $high = Buf.new(0xE1, 0xE2, 0xB5);   # latin-1 for a-acute, a-circumflex, micro

{
    my $d = Encoding::Registry.find('iso-8859-1').decoder();
    $d.add-bytes($high);
    is $d.consume-all-chars(), "\c[LATIN SMALL LETTER A WITH ACUTE]\c[LATIN SMALL LETTER A WITH CIRCUMFLEX]\c[MICRO SIGN]",
        'consume-all-chars honours a latin-1 decoder';
}

{
    my $d = Encoding::Registry.find('latin-1').decoder();
    $d.add-bytes($high);
    is $d.consume-available-chars().chars, 3,
        'consume-available-chars hands back every byte of a single-byte encoding';
}

{
    my $d = Encoding::Registry.find('iso-8859-1').decoder();
    $d.set-line-separators(["\r\n", "\n"]);
    $d.add-bytes(Buf.new(0xE1, 0x0D, 0x0A, 0xE2, 0x0D, 0x0A));
    is $d.consume-line-chars(:chomp), "\c[LATIN SMALL LETTER A WITH ACUTE]",
        'consume-line-chars decodes a latin-1 line';
    is $d.consume-line-chars(:chomp), "\c[LATIN SMALL LETTER A WITH CIRCUMFLEX]",
        'and the next one';
    nok $d.consume-line-chars(:chomp).defined, 'then an undefined Str';
}

# UTF-8 stays intact, including the incomplete-tail handling
# `consume-available-chars` needs for a multi-byte encoding.
{
    my $d = Encoding::Registry.find('utf-8').decoder();
    $d.add-bytes("h\c[LATIN SMALL LETTER E WITH ACUTE]llo".encode('utf-8'));
    is $d.consume-all-chars(), "h\c[LATIN SMALL LETTER E WITH ACUTE]llo",
        'a utf-8 decoder still decodes utf-8';
}

{
    my $bytes = "\c[LATIN SMALL LETTER E WITH ACUTE]x".encode('utf-8');
    my $d = Encoding::Registry.find('utf-8').decoder();
    $d.add-bytes($bytes.subbuf(0, 1));           # first half of the 2-byte sequence
    is $d.consume-available-chars(), '',
        'an incomplete utf-8 sequence is held back';
    # (`consume-available-chars` alone is not pinned here: raku holds the final
    # grapheme back in case a combining mark follows, mutsu does not.)
    $d.add-bytes($bytes.subbuf(1));
    is $d.consume-all-chars(), "\c[LATIN SMALL LETTER E WITH ACUTE]x",
        'and completes once the rest arrives';
}

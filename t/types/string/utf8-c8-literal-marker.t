use Test;

# A utf8-c8 synthetic grapheme is distinct from the literal text U+10FFFD 'x' H H
# (ADR-17800, LibTmux's invalid-byte check).

plan 14;

my $syn = Blob.new(255).decode('utf8-c8');
is $syn.chars, 1, 'the synthetic is one grapheme';
is $syn.ords, (0x10FFFD, 0x78, 0x46, 0x46), 'its exposed codepoints';
is $syn.encode('utf8-c8'), Blob.new(255), 'it round-trips through utf8-c8';
is $syn.encode('utf8'), Blob.new(0xF4, 0x8F, 0xBF, 0xBD, 0x78, 0x46, 0x46), 'its utf8 encoding';

my $text = $syn.encode('utf8').decode('utf8');
is $text.chars, 4, 'the same codepoints decoded as utf8 are four graphemes';
nok $text eq $syn, 'and are not the synthetic';
is $text.ords, (0x10FFFD, 0x78, 0x46, 0x46), 'with the same codepoints';

is "\x[10FFFD]xFF".chars, 4, 'a \x escape is literal text';
is (chr(0x10FFFD) ~ 'xFF').chars, 4, 'so is chr';
is "\x[10FFFD]".ords, (0x10FFFD,), 'a literal U+10FFFD is one codepoint';
is "\x[10FFFD]".chars, 1, 'and one grapheme';
is "\x[10FFFD]".encode, Blob.new(0xF4, 0x8F, 0xBF, 0xBD), 'and encodes as itself';
is Blob.new(0xF4, 0x8F, 0xBF, 0xBD).decode('utf8-c8').encode('utf8-c8'),
    Blob.new(0xF4, 0x8F, 0xBF, 0xBD), 'a valid U+10FFFD round-trips through utf8-c8';
is Blob.new(0xF4, 0x8F, 0xBF, 0xBD).decode.ord, 0x10FFFD, 'decoded, it reads back';

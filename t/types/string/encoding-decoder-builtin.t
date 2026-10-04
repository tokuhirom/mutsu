use Test;

# Encoding::Decoder::Builtin, the class Rakudo builds over the nqp::decoder*
# ops (#11503): its shape, its constructor, and the methods the streaming
# state machine gained.

plan 16;

my $d = Encoding::Registry.find('utf8').decoder;
is $d.^name, 'Encoding::Decoder::Builtin', '.decoder makes an Encoding::Decoder::Builtin';
ok $d ~~ Encoding::Decoder, 'which does Encoding::Decoder';

{
    my $n = Encoding::Decoder::Builtin.new('utf8', :translate-nl);
    $n.add-bytes("a\r\nb".encode);
    is $n.consume-all-chars, "a\nb", '.new takes the encoding and :translate-nl';
}

{
    my $c = Encoding::Registry.find('utf8').decoder;
    $c.add-bytes('abcd'.encode);
    is $c.consume-exactly-chars(2), 'ab', 'consume-exactly-chars';
    nok $c.consume-exactly-chars(5).defined, 'too few chars: an undefined Str';
    is $c.consume-exactly-chars(5, :eof), 'cd', ':eof takes what is left';
}

{
    # The last grapheme waits for a possible combining mark.
    my $g = Encoding::Registry.find('utf8').decoder;
    $g.add-bytes('e'.encode);
    is $g.consume-available-chars, '', 'a lone base char is held back';
    nok $g.is-empty, 'so the decoder is not empty';
    $g.add-bytes("\x[301]".encode);
    is $g.consume-all-chars, "\c[LATIN SMALL LETTER E WITH ACUTE]",
        'the combining mark joins it';
    ok $g.is-empty, 'now it is';
}

{
    # Header lines, then body bytes: the bytes behind a line stay raw.
    my $h = Encoding::Registry.find('iso-8859-1').decoder;
    $h.set-line-separators(["\r\n", "\n"]);
    $h.add-bytes("Length: 3\r\n\r\n\x[E9]\x[FF]!".encode('latin-1'));
    is $h.consume-line-chars(:chomp), 'Length: 3', 'a header line';
    is $h.consume-line-chars(:chomp), '', 'the blank line';
    is $h.bytes-available, 3, 'the body is still bytes';
    is $h.consume-exactly-bytes(3).list, (0xE9, 0xFF, 0x21), 'and comes out raw';
}

{
    # A decoder held in an attribute keeps its state across calls.
    my class Reader {
        has $.decoder = Encoding::Registry.find('utf8').decoder;
    }
    my $r = Reader.new;
    $r.decoder.add-bytes("one\ntw".encode);
    is $r.decoder.consume-line-chars(:chomp), 'one', 'through an accessor';
    $r.decoder.add-bytes("o\n".encode);
    is $r.decoder.consume-line-chars(:chomp), 'two', 'and again after more bytes';
}

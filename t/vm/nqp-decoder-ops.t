use Test;
use nqp;

# The stream-decoding nqp:: ops (#11503), checked against rakudo 2026.09.
# They share one state machine with Encoding::Decoder::Builtin's methods.

plan 39;

sub decoder(Str $enc, *%config) {
    my $d := nqp::create(Encoding::Decoder::Builtin);
    my $h := nqp::hash();
    nqp::bindkey($h, $_.key, $_.value) for %config;
    nqp::decoderconfigure($d, $enc, $h);
    $d
}

{
    my $d := decoder('utf8');
    nqp::decoderaddbytes($d, "héllo\nx".encode);
    is nqp::decoderbytesavailable($d), 8, 'bytes are queued undecoded';
    is nqp::decoderempty($d), 0, 'decoderempty with bytes queued';
    is nqp::decodertakeline($d, 1, 0), 'héllo', 'decodertakeline chomps';
    is nqp::decoderbytesavailable($d), 1, 'the bytes behind the line stay undecoded';
    ok nqp::isnull_s(nqp::decodertakeline($d, 1, 0)), 'no complete line: null';
    is nqp::decoderbytesavailable($d), 0, 'the search decoded the rest';
    is nqp::decodertakeline($d, 1, 1), 'x', 'incomplete-ok takes the last line';
    is nqp::decoderempty($d), 1, 'then the decoder is empty';
}

{
    # The last grapheme is held back: a combining mark could still join it.
    my $d := decoder('utf8');
    nqp::decoderaddbytes($d, Blob.new(0x61));
    is nqp::decodertakeavailablechars($d), '', 'a lone base char is held';
    is nqp::decoderempty($d), 0, 'held chars keep the decoder non-empty';
    is nqp::decoderbytesavailable($d), 0, 'though no bytes are left';
    nqp::decoderaddbytes($d, Blob.new(0xCC, 0x81, 0x62));
    is nqp::decodertakeavailablechars($d), "\c[LATIN SMALL LETTER A WITH ACUTE]",
        'the combining mark joins it (NFC)';
    is nqp::decodertakeallchars($d), 'b', 'decodertakeallchars releases the held char';
}

{
    # A multi-byte character split across two decoderaddbytes calls.
    my $d := decoder('utf8');
    nqp::decoderaddbytes($d, Blob.new(0xC3));
    is nqp::decoderbytesavailable($d), 1, 'half a character stays a byte';
    is nqp::decodertakeavailablechars($d), '', 'and decodes to nothing yet';
    nqp::decoderaddbytes($d, Blob.new(0xA9, 0x41, 0x42, 0x43));
    is nqp::decodertakechars($d, 2), 'éA', 'decodertakechars once it completes';
    ok nqp::isnull_s(nqp::decodertakechars($d, 5)), 'too few chars: null';
    is nqp::decodertakechars($d, 1), 'B', 'one more';
    ok nqp::isnull_s(nqp::decodertakechars($d, 1)), 'the held last char is not available';
    is nqp::decodertakecharseof($d, 5), 'C', 'decodertakecharseof takes what is left';
}

{
    my $d := decoder('utf8');
    nqp::decoderaddbytes($d, Blob.new(1, 2, 3, 4));
    is nqp::decodertakebytes($d, nqp::create(buf8.new.WHAT), 3), Buf[uint8].new(1, 2, 3),
        'decodertakebytes';
    ok nqp::isnull(nqp::decodertakebytes($d, nqp::create(buf8.new.WHAT), 3)),
        'too few bytes: null';
    is nqp::decoderbytesavailable($d), 1, 'and nothing taken';
    isa-ok nqp::decodertakebytes($d, nqp::create(blob8.new.WHAT), 1), Blob[uint8],
        'the buffer type is the one passed';
}

{
    my $d := decoder('utf8');
    nqp::decodersetlineseps($d, nqp::list_s('ab', "\n"));
    nqp::decoderaddbytes($d, "xabyy\nzz".encode);
    is nqp::decodertakeline($d, 0, 0), 'xab', 'a custom separator, kept';
    is nqp::decodertakeline($d, 1, 0), 'yy', 'the earliest separator wins';
    is nqp::decodertakeline($d, 0, 1), 'zz', 'the rest at the end of the stream';
}

{
    # A trailing \r waits: a \n would make it the \r\n grapheme.
    my $d := decoder('utf8');
    nqp::decoderaddbytes($d, "a\r".encode);
    ok nqp::isnull_s(nqp::decodertakeline($d, 1, 0)), 'a trailing \r is not yet a line end';
    nqp::decoderaddbytes($d, "\nb".encode);
    is nqp::decodertakeline($d, 1, 0), 'a', 'the \r\n completes the line';
}

{
    my $d := decoder('utf8', translate_newlines => 1);
    nqp::decoderaddbytes($d, "a\r\nb\rc".encode);
    is nqp::decodertakeallchars($d), "a\nb\rc", 'translate_newlines turns \r\n into \n';
}

{
    my $d := decoder('iso-8859-1');
    nqp::decoderaddbytes($d, Blob.new(0xE9, 0x0A, 0x41));
    is nqp::decodertakeline($d, 0, 0), "é\n", 'a latin-1 decoder';
}

{
    my $d := decoder('utf8');
    nqp::decoderaddbytes($d, Blob.new(0x61, 0xC3));
    throws-like { nqp::decodertakeallchars($d) }, Exception,
        message => 'Incomplete character near bytes c3 at the end of a stream',
        'an incomplete character at the end is an error';
    my $e := decoder('utf8');
    nqp::decoderaddbytes($e, Blob.new(0x61, 0xFF));
    throws-like { nqp::decodertakeallchars($e) }, Exception,
        message => 'Malformed UTF-8 near byte ff', 'malformed UTF-8 is an error';
}

{
    my $d := nqp::create(Encoding::Decoder::Builtin);
    throws-like { nqp::decodertakeallchars($d) }, Exception,
        message => 'Decoder not yet configured', 'an unconfigured decoder';
    nqp::decoderconfigure($d, 'utf8', nqp::hash());
    throws-like { nqp::decoderconfigure($d, 'utf8', nqp::hash()) }, Exception,
        message => 'Decoder already configured', 'configuring twice';
    throws-like { nqp::decoderconfigure(nqp::create(Encoding::Decoder::Builtin), 'bogus', nqp::hash()) },
        Exception, message => "Unknown string encoding: 'bogus'", 'an unknown encoding';
}

{
    # The ops and the methods share one decoder state.
    my $d := Encoding::Registry.find('utf8').decoder;
    isa-ok $d, Encoding::Decoder::Builtin, '.decoder makes an Encoding::Decoder::Builtin';
    nqp::decoderaddbytes($d, "one\ntwo\n".encode);
    is $d.consume-line-chars(:chomp), 'one', 'a method reads what an op added';
    is nqp::decodertakeline($d, 1, 0), 'two', 'and an op reads on after the method';
}

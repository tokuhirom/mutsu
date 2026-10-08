use Test;

plan 4;

# IO::Handle text reads run on the streaming decoder (#11783), so malformed
# and truncated input fails with the decoder's MoarVM-style messages.

my $f = $*TMPDIR.add("mutsu-malformed-utf8-{$*PID}.dat");
LEAVE $f.unlink;

$f.spurt(Blob.new(0x61, 0xff, 0x0a));
{
    my $h = $f.open;
    my $msg = '';
    try { $h.get; CATCH { default { $msg = .message } } }
    is $msg, 'Malformed UTF-8 near byte ff', 'invalid byte names the byte';
    $h.close;
}

$f.spurt(Blob.new(0x61, 0x0a, 0xe3, 0x81));
{
    my $h = $f.open;
    is $h.get, 'a', 'the complete line before the truncated tail reads fine';
    my $msg = '';
    try { $h.get; CATCH { default { $msg = .message } } }
    is $msg, 'Incomplete character near bytes e3 81 at the end of a stream',
        'truncated sequence at end of stream';
    $h.close;
}

$f.spurt(Blob.new(0x65, 0xcc, 0x81, 0x0a));
is $f.open.get.chars, 1, 'a decomposed grapheme is read as one character';

# vim: expandtab shiftwidth=4

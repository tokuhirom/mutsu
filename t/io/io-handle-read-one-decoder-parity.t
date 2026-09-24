use v6;
use Test;

# An IO handle's text reads (`.get`, `.lines`, `.slurp`, `.readchars`,
# `.getc`, `.words`) decode through the same one decoder as `Blob.decode`
# and `IO::Path.slurp` (ADR-0118 §2.4; #9226): the result is NFC-normalized,
# and invalid UTF-8 dies instead of turning into U+FFFD. The handle paths used
# to call `String::from_utf8_lossy` directly. Expected values were measured with
# rakudo.

plan 14;

my $path = $*TMPDIR.add("mutsu-handle-decode-parity-{$*PID}.txt");
LEAVE { $path.unlink if $path.e }

# U+2126 OHM SIGN, then a newline; its NFC form is U+03A9.
$path.spurt(Buf.new(0xE2, 0x84, 0xA6, 0x0A), :bin);
{
    my $h = $path.open;
    is $h.get.ord, 0x3A9, '.get normalizes to NFC';
    $h.close;
}
is $path.open.slurp.ord, 0x3A9, 'a handle .slurp normalizes';
is $path.open.readchars(1).ord, 0x3A9, '.readchars(1) normalizes';
is $path.open.getc.ord, 0x3A9, '.getc normalizes';
is $path.open.lines[0].ord, 0x3A9, 'a handle .lines normalizes';
is $path.open.words[0].ord, 0x3A9, 'a handle .words normalizes';
is $path.lines[0].ord, 0x3A9, 'IO::Path.lines agrees (it already used the decoder)';
is $path.open(:bin).read.decode.ord, 0x3A9, '.read.decode agrees';

# A decomposed "e" + COMBINING ACUTE composes to U+00E9.
$path.spurt("e\x[301]x\n");
is-deeply $path.open.readchars(1).ords.List, (0xE9,), '.readchars reads one composed grapheme';
is-deeply $path.open.get.ords.List, (0xE9, 0x78), '.get composes the line';

# Invalid UTF-8 dies, as rakudo's decoder does.
$path.spurt(Buf.new(0x61, 0xFF, 0x62, 0x0A), :bin);
sub dies-malformed(&code, $desc) {
    try { sink code() }
    ok ($! && $!.message.starts-with('Malformed UTF-8')), $desc;
}
dies-malformed { $path.open.get }, '.get on invalid UTF-8 dies';
dies-malformed { $path.open.slurp }, 'a handle .slurp on invalid UTF-8 dies';
dies-malformed { $path.open.lines.eager }, 'a handle .lines on invalid UTF-8 dies';

# Plain ASCII is untouched.
$path.spurt("abc\ndef\n");
is-deeply $path.open.lines.List, ("abc", "def"), 'valid ASCII lines read as before';

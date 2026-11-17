use Test;
use nqp;

# ADR-17800 slice 2: a literal U+10FFFD that enters a Str through a file
# read, a handle read, the stream decoder, `chrs`, `Uni.Str` or `nqp::chr`
# is not a utf8-c8 synthetic, so `U+10FFFD x F F` built that way is four
# graphemes; the normalization forms expose it as one codepoint.

plan 17;

my $bytes = Blob.new(0xF4, 0x8F, 0xBF, 0xBD, 0x78, 0x46, 0x46);
my $file = $*TMPDIR.add("mutsu-c8-marker-$*PID.txt");
$file.spurt: $bytes;

is slurp($file).chars, 4, 'slurp of a file';
is $file.slurp.chars, 4, 'IO::Path.slurp';
is $file.lines.head.chars, 4, 'IO::Path.lines';
{
    my $h = open $file;
    is $h.readchars(4).chars, 4, 'IO::Handle.readchars';
    $h.close;
}
{
    my $h = open $file;
    is $h.get.chars, 4, 'IO::Handle.get';
    $h.close;
}
{
    my $h = open $file;
    is-deeply $h.getc.ords, (0x10FFFD,), 'IO::Handle.getc reads the literal marker alone';
    $h.close;
}
{
    my $h = open $file, :enc<utf8-c8>;
    is $h.slurp.chars, 4, 'a utf8-c8 handle keeps a valid marker literal';
    $h.close;
}
$file.unlink;

{
    my $d = Encoding::Registry.find("utf8").decoder;
    $d.add-bytes($bytes);
    is $d.consume-available-chars.chars, 3, 'stream decoder holds back only the last grapheme';
    is $d.consume-all-chars.chars, 1, 'and releases it at the end';
}

is (0x10FFFD, 0x78, 0x46, 0x46).chrs.chars, 4, '.chrs';
is chrs(0x10FFFD, 0x78, 0x46, 0x46).chars, 4, 'chrs()';
is Uni.new(0x10FFFD, 0x78, 0x46, 0x46).Str.chars, 4, 'Uni.Str';
is (nqp::chr(0x10FFFD) ~ "xFF").chars, 4, 'nqp::chr';
is nqp::codes(nqp::chr(0x10FFFD)), 1, 'nqp::codes counts the literal once';

is-deeply "\x[10FFFD]".NFC.list, (0x10FFFD,), '.NFC exposes one codepoint';
is-deeply "\x[10FFFD]xFF".NFKD.list, (0x10FFFD, 0x78, 0x46, 0x46), '.NFKD';
is-deeply Blob.new(255).decode("utf8-c8").NFD.list, (0x10FFFD, 0x78, 0x46, 0x46),
    'a real synthetic still exposes its four codepoints';

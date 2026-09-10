use Test;

plan 12;

# A text-mode read decodes the CRLF line ending to a single "\n" (the handle's
# default `nl-in` is `["\n", "\r\n"]`). Binary reads and `Blob.decode` keep the
# bytes verbatim, and a lone "\r" is not a line ending so it survives.

my $path = $*TMPDIR.add("mutsu-crlf-{$*PID}.txt");
LEAVE { $path.unlink if $path.e }

$path.spurt: Buf.new("a\r\nb\r\nc".encode('utf-8').list);

is $path.slurp, "a\nb\nc", 'IO::Path.slurp translates CRLF to LF';
is slurp($path.Str), "a\nb\nc", 'the slurp sub translates CRLF to LF';
is-deeply $path.slurp(:bin).list.map(*.Int).Array, [97, 13, 10, 98, 13, 10, 99].Array,
    ':bin keeps the bytes verbatim';
is $path.slurp(:bin).decode, "a\r\nb\r\nc", 'Blob.decode does not translate';

is-deeply $path.lines.Array, ["a", "b", "c"], 'IO::Path.lines splits on the translated ending';
is-deeply $path.comb.Array, ["a", "\n", "b", "\n", "c"], 'IO::Path.comb sees the translated content';

my $fh = $path.open;
is $fh.slurp, "a\nb\nc", 'IO::Handle.slurp translates CRLF to LF';
$fh.close;

$fh = $path.open;
is $fh.get, "a", 'IO::Handle.get chomps the CRLF ending';
is $fh.get, "b", 'the following get is positioned correctly';
$fh.close;

# `\r\n` is a single grapheme cluster, so a bounded character read gets the
# requested count back after the translation.
$fh = $path.open;
is $fh.readchars(4), "a\nb\n", 'readchars counts the translated newline as one char';
$fh.close;

# A lone CR is not a line ending.
$path.spurt: Buf.new("x\ry\r\nz".encode('utf-8').list);
is $path.slurp, "x\ry\nz", 'a lone CR survives the translation';

# Writing is unaffected: spurt/print emit exactly what they are given.
$path.spurt: "p\r\nq";
is-deeply $path.slurp(:bin).list.map(*.Int).Array, [112, 13, 10, 113].Array,
    'spurt writes CRLF through unchanged';

use Test;

# An interpolation of plain Str and Int pieces whose result is short (under
# the strand threshold) is written into one buffer directly; a longer one is
# still joined by the strand-aware joiner. Both must give the same text,
# composed to NFC across every seam.

plan 7;

my $i = 12345;
my $s = 'key';
is "$s-$i", 'key-12345', 'Str and Int pieces';

# A combining mark at the start of a piece composes with the end of the one
# before it.
my $e = 'e';
my $acute = "\x[301]";
is "$e$acute".NFC.list, "\x[e9]".NFC.list, 'NFC across a seam';
is "$e$acute".chars, 1, 'one grapheme across the seam';

# Results just under and over the strand threshold (1024 bytes).
my $big = 'x' x 1000;
is "$big$i".chars, 1005, 'a short result with a large piece';
my $bigger = 'y' x 1100;
is "$bigger$i".chars, 1105, 'a result over the strand threshold';
is "$bigger$e$acute".substr(*-1), "\x[e9]", 'NFC across a seam in a long result';

# An empty Str piece.
my $empty = '';
is "$empty$i$empty", '12345', 'empty pieces';

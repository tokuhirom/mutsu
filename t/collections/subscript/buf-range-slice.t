use Test;

# `$buf[$a..$b]` (slicing a Buf/Blob by a Range) returned Nil: a single-Int
# index was served by AT-POS but a Range index had no match arm and fell
# through. Raku yields the list of byte values in range. This blocked
# Humming-Bird's Request.decode body extraction
# (`Buf.new: $payload[$idx..($payload.bytes-1)].Slip`).

plan 8;

my $b = Buf.new("buy milk".encode);   # 98 117 121 32 109 105 108 107

is $b[0..7].elems, 8,                    'inclusive range slice length';
is $b[0..7].join(','), '98,117,121,32,109,105,108,107', 'inclusive range bytes';
is $b[2..^5].join(','), '121,32,109',    'exclusive-end range slice';
is $b[2^..5].join(','), '32,109,105',    'exclusive-start range slice';

my $i = 0; my $j = 7;
is $b[$i..$j].elems, 8,                  'range with variable endpoints';

# round-trip through Buf.new + .Slip (the decode idiom)
my $copy = Buf.new: $b[0..($b.bytes - 1)].Slip;
is $copy.bytes, 8,                       'sliced bytes rebuild a Buf';
is $copy.decode, 'buy milk',             'rebuilt Buf decodes to original';

# single-index still works
is $b[1], 117,                           'single-Int index still works';

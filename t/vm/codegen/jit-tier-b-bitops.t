use v6;
use Test;

# `+&`, `+|` and `+^` have a Tier B inline path for two small Ints (#19406).
# Hot loops over every operand shape must answer as rakudo does: small Ints of
# both signs, values at the edge of the small-Int range, big Ints, and
# non-Int operands that coerce. A user infix declared later must still be
# honored by the compiled loop; that part runs in a child process, since the
# guard (`USER_INFIX_DECLS`) is process-wide and monotonic.

plan 9;

my @small = 0, 1, -1, 255, -256, 12345, -98765;
my @edge = 2**47 - 1, -2**47, 2**46, -2**46 - 3;
my @big = 2**70 + 5, -2**64 - 1, 2**48;
my ($and, $or, $xor, $eor, $exor, $band) = 0 xx 6;
for ^300 -> $i {
    my $s = @small[$i % 7];
    $and += ($s +& $i);
    $or  += ($s +| $i);
    $xor += ($s +^ $i);
    my $e = @edge[$i % 4];
    $eor  += ($e +| $i);
    $exor += ($e +^ $i);
    $band += (@big[$i % 3] +& $i);
}
is $and, 15332, '+& on small Ints in a hot loop';
is $or, -3587820, '+| on small Ints in a hot loop';
is $xor, -3603152, '+^ on small Ints in a hot loop';
is $eor, 22275, '+| at the small-Int boundary';
is $exor, 0, '+^ at the small-Int boundary';
is $band, 15196, '+& with big Ints falls back and stays right';

my $acc = 0;
for ^300 -> $i { $acc += ($i +& 7) + ("12" +| $i) + (3.9 +^ $i) }
is $acc, 92566, 'Str and Rat operands coerce in a hot loop';

my $child = q:to/RAKU/;
    my $s = 0;
    for ^300 -> $i { $s += $i +& 3 }
    say $s;
    multi sub infix:<+&>(Int $a, Int $b where * == 3) { 100 }
    my $t = 0;
    for ^300 -> $i { $t += $i +& 3 }
    say $t;
    RAKU
my $proc = run($*EXECUTABLE, '-e', $child, :out, :err);
my @lines = $proc.out.slurp(:close).lines;
is @lines[0], '30000', 'a lexically visible user infix:<+&> wins from the first iteration';
is @lines[1], '30000', 'and keeps winning once the loop is compiled';

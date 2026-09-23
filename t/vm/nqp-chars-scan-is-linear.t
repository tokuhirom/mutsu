use Test;
use nqp;

plan 7;

# `nqp::chars` used to copy its string argument and count it on every call,
# so nqp's idiomatic scanner loop --
#
#     nqp::while(nqp::islt_i($i, nqp::chars($s)), ...)
#
# -- which re-asks the length on every iteration, ran in O(n^2) (#9130).
# A long string now answers from its cached per-payload index.

is nqp::chars(''), 0, 'nqp::chars of the empty string';
is nqp::chars('hello'), 5, 'nqp::chars of a short ASCII string';
is nqp::chars('日本語'), 3, 'nqp::chars of a short non-ASCII string';

my $long = '日本語' x 200;
is nqp::chars($long), 600, 'nqp::chars of a long (cached) non-ASCII string';
is nqp::chars($long), 600, 'asking again answers the same';
is nqp::chars(12345), 5, 'nqp::chars stringifies a non-Str argument';

# The scan itself, timed at two sizes. The assertion is a *ratio* between two
# runs in one process, so machine speed and CI load cancel out. Quadratic
# would be ~16x for a 4x longer input; linear is ~4x.
sub scan-time(int $n --> Num) {
    my $s = 'a' x $n;
    my $t0 = now;
    my int $i = -1;
    my int $sum = 0;
    nqp::while(
      nqp::islt_i(++$i, nqp::chars($s)),
      ($sum = nqp::add_i($sum, 1))
    );
    (now - $t0).Num;
}

scan-time(5000);   # warm up, so the first timed run pays no one-off cost
my $small = scan-time(5000);
my $large = scan-time(20000);
my $ratio = $large / ($small || 1e-9);
ok $ratio < 9, "scanning 4x the characters costs ~4x, not ~16x (ratio $ratio.fmt('%.2f'))";

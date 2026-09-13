use Test;
use nqp;

plan 7;

# `nqp::elems` and `nqp::atpos_i` used to answer from a whole-vector copy of
# the target's elements, so nqp's idiomatic scan --
#
#     while $i < nqp::elems($codes) { ... nqp::atpos_i($codes, $i) ... }
#
# -- paid two O(n) copies per codepoint and ran in O(n^2). `JSON::Fast`'s
# `str-escape` is exactly that loop over `text.NFD`, which made encoding one
# long string quadratic in its length (#8289).

my $codes := 'hello'.NFD;
is nqp::elems($codes), 5, 'nqp::elems answers a Uni codepoint count';
is nqp::atpos_i($codes, 0), 104, 'nqp::atpos_i reads the first codepoint';
is nqp::atpos_i($codes, 4), 111, 'nqp::atpos_i reads the last codepoint';

my $wide := "\c[LATIN SMALL LETTER E WITH ACUTE]x".NFD;
is nqp::elems($wide), 3, 'a decomposed codepoint counts as its NFD elements';
is nqp::atpos_i($wide, 2), 120, 'nqp::atpos_i indexes past a decomposition';

# The scan itself, timed at two sizes. The assertion is a *ratio* between two
# runs in one process, so machine speed and CI load cancel out rather than
# setting the threshold. Quadratic would be ~16x for a 4x longer input;
# linear is ~4x. Assert well below the quadratic figure and well above the
# linear one, so only a genuine complexity regression trips it.
sub scan-time(int $n --> Num) {
    my $codes := ('x' x $n).NFD;
    my $t0 = now;
    my int $i = -1;
    my int $sum = 0;
    nqp::while(
      nqp::islt_i(++$i, nqp::elems($codes)),
      ($sum = nqp::add_i($sum, nqp::atpos_i($codes, $i)))
    );
    (now - $t0).Num;
}

scan-time(2000);   # warm up, so the first timed run pays no one-off cost
my $small = scan-time(2000);
my $large = scan-time(8000);

# Guard against a clock so coarse the ratio is meaningless.
ok $small > 0, 'the short scan took measurable time';
my $ratio = $large / ($small || 1e-9);
ok $ratio < 9, "scanning 4x the codepoints costs ~4x, not ~16x (ratio $ratio.fmt('%.2f'))";

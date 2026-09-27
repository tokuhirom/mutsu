use Test;
use nqp;

plan 20;

# `nqp::radix($base, $s, $pos, $flags)` used to copy the whole string and
# collect every grapheme into a Vec on each call, so a tokenizer calling it at
# successive positions was O(n^2) (#9131). It now walks forward from `$pos`
# over the cached grapheme index and costs the digits consumed.

sub r($base, $s, $pos, $flags) {
    my $r := nqp::radix($base, $s, $pos, $flags);
    [nqp::atpos($r, 0), nqp::atpos($r, 1), nqp::atpos($r, 2)]
}

is-deeply r(10, '123', 0, 0), [123, 3, 3], 'plain decimal';
is-deeply r(10, '1_2_3', 0, 0), [123, 3, 5], 'single underscores between digits are consumed';
is-deeply r(10, '1__2', 0, 0), [1, 1, 1], 'a double underscore stops the scan before it';
is-deeply r(10, '1_', 0, 0), [1, 1, 1], 'a trailing underscore is not consumed';
is-deeply r(10, '-12', 0, 2), [-12, 2, 3], 'flag 2 parses a leading minus';
is-deeply r(10, '+12', 0, 2), [12, 2, 3], 'flag 2 parses a leading plus';
is-deeply r(10, '-12', 0, 0), [0, 0, -1], 'without flag 2 a sign is not a digit';
is-deeply r(10, '-', 0, 2), [0, 0, -1], 'a lone sign is no match';
is-deeply r(10, '1200', 0, 4), [12, 2, 4], 'flag 4 drops trailing zeroes but consumes them';
is-deeply r(10, '000', 0, 4), [0, 0, 3], 'flag 4 on all zeroes';
is-deeply r(10, '12', 0, 1), [-12, 2, 2], 'flag 1 negates';
is-deeply r(16, 'fF_a', 0, 0), [4090, 3, 4], 'hex digits of either case';
is-deeply r(10, 'ab12cd', 2, 0), [12, 2, 4], 'scanning starts at $pos';
is-deeply r(10, 'abc', 5, 0), [0, 0, -1], '$pos past the end is no match';
is-deeply r(10, "é12", 1, 0), [12, 2, 3], '$pos and the offset are grapheme positions';
is-deeply r(10, "\r\n12", 1, 0), [12, 2, 3], '\r\n is one grapheme';
is-deeply r(10, 'x٣4', 1, 0), [34, 2, 3], 'any Unicode Nd digit counts';
is-deeply r(36, 'Ｚｚ', 0, 0), [1295, 2, 2], 'fullwidth letters are digits';

# The scan itself, timed at two sizes. The assertion is a *ratio* between two
# runs in one process, so machine speed and CI load cancel out. Quadratic
# would be ~16x for a 4x longer input; linear is ~4x.
sub scan-time(int $n --> Num) {
    my $s = '1 ' x $n;
    my $t0 = now;
    my int $i = -1;
    nqp::while(nqp::islt_i(++$i, $n), nqp::radix(10, $s, nqp::mul_i(2, $i), 0));
    (now - $t0).Num;
}

scan-time(5000);   # warm up, so the first timed run pays no one-off cost
my $small = scan-time(5000);
my $large = scan-time(20000);
my $ratio = $large / ($small || 1e-9);
ok $ratio < 9, "tokenizing 4x the text costs ~4x, not ~16x (ratio $ratio.fmt('%.2f'))";

my $long = '日本語 42 ' x 100;
is-deeply r(10, $long, 4, 0), [42, 2, 6], 'a long non-ASCII (cached-index) string scans from $pos';

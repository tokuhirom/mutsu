use Test;
use nqp;

plan 17;

# Removing the first element used to be `Vec::remove(0)`, a memmove of the
# whole remaining array, so consuming a list from the front was quadratic.
# JSON::Fast's `unjsonify-string` `nqp::shift_i`s every codepoint off a `Uni`,
# which made decoding a long escaped string quadratic in its length (#9121).
# The array now keeps a head offset (as MoarVM's VMArray does), so these pin
# both that the offset is invisible to every other operation and that the
# shift loop is linear.

{
    my @a = 1..10;
    is @a.shift, 1, 'shift answers the first element';
    is @a.shift, 2, 'a second shift answers the next one';
    is @a.elems, 8, 'elems counts only the live elements';
    is @a[0], 3, 'index 0 is the new front';
    is @a.join(','), '3,4,5,6,7,8,9,10', 'iteration starts at the new front';
    @a.push(11);
    @a.unshift(0);
    is @a.join(','), '0,3,4,5,6,7,8,9,10,11', 'push and unshift after shifts';
    is @a.pop, 11, 'pop after shifts answers the last element';
    ok @a eqv [0, 3, 4, 5, 6, 7, 8, 9, 10], 'eqv compares only the live elements';
    my @b = @a.clone;
    @a.shift for ^5;
    is @b.join(','), '0,3,4,5,6,7,8,9,10', 'a clone taken before shifts is unaffected';
    is @a.join(','), '7,8,9,10', 'the original keeps shifting';
    @a.shift for ^4;
    is @a.elems, 0, 'shifting everything empties the array';
    @a.push(42);
    is @a.join(','), '42', 'an emptied array can be refilled';
}

{
    my @q = 1, 2, 3;
    my @seen;
    while @q {
        my $x = @q.shift;
        @seen.push($x);
        @q.push($x + 10) if $x < 10;
    }
    is @seen.join(','), '1,2,3,11,12,13', 'a queue that interleaves shift and push';
}

{
    my $l := nqp::list(1, 2, 3, 4);
    is nqp::shift($l), 1, 'nqp::shift answers the first element';
    is nqp::elems($l), 3, 'nqp::elems after nqp::shift';
    is nqp::atpos($l, 0), 2, 'nqp::atpos(0) is the new front';
}

# The consuming loop itself, timed at two sizes. The assertion is a *ratio*
# between two runs in one process, so machine speed and CI load cancel out.
# Quadratic would be ~16x for a 4x longer input; linear is ~4x.
sub drain-time(int $n --> Num) {
    my $codes := ('x' x $n).NFD;
    my $t0 = now;
    my int $sum = 0;
    nqp::while(nqp::elems($codes), ($sum = nqp::add_i($sum, nqp::shift_i($codes))));
    die "bad sum" unless $sum == 120 * $n;
    (now - $t0).Num;
}

drain-time(5000);   # warm up, so the first timed run pays no one-off cost
my $small = drain-time(5000);
my $large = drain-time(20000);
my $ratio = $large / ($small || 1e-9);
ok $ratio < 9, "draining 4x the codepoints costs ~4x, not ~16x (ratio $ratio.fmt('%.2f'))";

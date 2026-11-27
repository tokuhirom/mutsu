use Test;
use nqp;

# TRIR fuses `$x = nqp::add_i($x, c)` (and `nqp::sub_i`) into one op, on a
# native slot and through an `int $pos is rw` reference (#8673). Each shape
# here must answer exactly what the unfused stack form did: native `int`
# arithmetic wraps, a subtraction is an addition of `-c`, a store into a
# DIFFERENT variable is not an in-place add, and an inlined callee's reference
# becomes the caller's own variable.

plan 9;

sub advance(int $pos is rw, int $n --> Nil) {
    my int $i = 0;
    nqp::while(
      nqp::islt_i($i, $n),
      nqp::stmts(
        ($pos = nqp::add_i($pos, 1)),
        ($i = nqp::add_i($i, 1))
      )
    );
}

sub back-two(int $pos is rw --> int) {
    $pos = nqp::sub_i($pos, 2);
    nqp::add_i($pos, 10)
}

sub copy-plus(int $a --> int) {
    my int $b = nqp::add_i($a, 7);
    nqp::sub_i($b, 3)
}

sub wraps(int $x --> int) {
    my int $y = $x;
    $y = nqp::add_i($y, 1);
    $y
}

sub big-constant(int $x --> int) {
    # Does not fit the fused op's i32 constant: stays the stack form.
    nqp::add_i($x, 4294967296)
}

sub min-i32(int $x --> int) {
    nqp::sub_i($x, -2147483648)
}

my int $p = 5;
advance($p, 3);
is $p, 8, 'an in-place add through an is rw reference';

my int $q = 20;
is back-two($q), 28, 'a sub through the reference, then an add used as a value';
is $q, 18, 'the reference was written by the sub';

is copy-plus(1), 5, 'an add into a different slot is not an in-place add';
is wraps(9223372036854775807), -9223372036854775808, 'native int adds wrap';
is big-constant(1), 4294967297, 'a constant outside i32 still adds';
is min-i32(0), 2147483648, 'subtracting i32::MIN still subtracts';

# The same loop through a nested call, so an inlined callee's reference is
# the caller's own variable.
sub outer-scan(int $n --> int) {
    my int $pos = 0;
    advance($pos, $n);
    $pos
}
is outer-scan(1000), 1000, 'an inlined callee adds into the caller variable';

my int $total = 0;
for ^50 { $total = nqp::add_i($total, outer-scan(3)) }
is $total, 150, 'repeated calls keep their own counters';

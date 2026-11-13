use Test;
use nqp;

# nqp::atpos / elems / push / shift on an IterationBuffer, and the operand
# shapes that must still take the general path (#17337).

plan 11;

my $ib := nqp::create(IterationBuffer);
is nqp::elems($ib), 0, 'a created IterationBuffer is empty';
is nqp::push($ib, 'a'), 'a', 'push answers the pushed value';
nqp::push($ib, $_) for <b c d>;
is nqp::elems($ib), 4, 'elems counts the pushed values';
is nqp::atpos($ib, 0), 'a', 'atpos reads the first element';
is nqp::atpos($ib, -1), 'd', 'a negative index counts from the end';
ok nqp::isnull(nqp::atpos($ib, 10)), 'atpos past the end is null';
dies-ok { nqp::atpos($ib, -10) }, 'atpos before the start dies';
is nqp::shift($ib), 'a', 'shift removes the first element';
is nqp::elems($ib), 3, 'and elems follows';

{
    my $i = 1;   # an Int in a container, not a native int
    is nqp::atpos($ib, $i), 'c', 'an index held in a scalar container';
}

{
    my $total = 0;
    my $buf := nqp::create(IterationBuffer);
    nqp::push($buf, $_) for ^100;
    my int $j = 0;
    nqp::while(nqp::islt_i($j, nqp::elems($buf)), nqp::stmts(
        ($total = $total + nqp::atpos($buf, $j)),
        ($j = nqp::add_i($j, 1))
    ));
    is $total, 4950, 'a loop over atpos sums every element';
}


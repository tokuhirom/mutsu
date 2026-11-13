use Test;
use nqp;

plan 11;

# nqp::slice of a list or an IterationBuffer: a new store of the same kind
# holding the elements start ..= end (MoarVM semantics). ParaSeq's .batch and
# .rotor slice their IterationBuffer batches with it.

my $ib := nqp::create(IterationBuffer);
nqp::push($ib, $_) for ^5;

my $s := nqp::slice($ib, 1, 3);
is $s.^name, 'IterationBuffer', 'slice of an IterationBuffer is one';
is-deeply $s.List, (1, 2, 3), 'start ..= end';
is-deeply nqp::slice($ib, 0, -1).List, (0, 1, 2, 3, 4), 'a negative end counts from the end';
is-deeply nqp::slice($ib, -2, -1).List, (3, 4), 'a negative start counts from the end';
is-deeply nqp::slice($ib, 4, 4).List, (4,), 'a single element';
is nqp::elems($ib), 5, 'the source is unchanged';

nqp::push($s, 99);
is nqp::elems($ib), 5, 'the slice does not share the source storage';

my $l := nqp::list(1, 2, 3, 4);
is nqp::elems(nqp::slice($l, 1, 2)), 2, 'slice of a list';

for (2, 10), (3, 1), (-1, 2) -> ($from, $to) {
    throws-like { nqp::slice($ib, $from, $to) }, Exception,
      message => 'MVMArray: Slice index out of bounds',
      "slice($from, $to) is out of bounds";
}

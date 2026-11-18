use Test;
use nqp;

# `nqp::clone` of a code object is a distinct copy that shares the original's
# body, `state` scope and `FIRST` flag (rakudo's clone shares `$!do`). A code
# object with a role mixed in also gets its own attribute slots, so a bind on
# the copy -- of a role attribute or of `Block.$!phasers` -- stays the copy's.
# ParaSeq's `BlockMapper` binds a `$!phasers` table on a copy per batch; mutsu
# returned the same object, so the batches overwrote one another's tables and
# the first batch's `FIRST` never ran. Expected values checked against raku.

plan 8;

my role R { has $.tag is rw; method rebind($v) { $!tag := $v } }

{
    my $blk = { state $n = 0; $n++ };
    my $c := nqp::clone($blk);
    nok $c =:= $blk, 'the copy is a distinct object';
    is ($blk(), $c(), $blk()).join, '012', 'the copy shares the state scope';
}

{
    my $m := ({ $_ * 2 } but R);
    $m.tag = 'orig';
    my $c := nqp::clone($m);
    is $c.tag, 'orig', 'the copy starts with the original attribute values';
    $c.rebind('copy');
    is $c.tag, 'copy', 'a bind on the copy is seen on the copy';
    is $m.tag, 'orig', 'a bind on the copy leaves the original alone';
    is (1, 2).map($c).List, (2, 4), 'the copy runs the original body';
}

{
    my $fired = 0;
    my $m := ({ FIRST $fired++; $_ } but R);
    my $first := nqp::clone($m);
    my $other := nqp::clone($m);
    my $table := nqp::clone(nqp::getattr($m, Block, '$!phasers'));
    nqp::deletekey($table, 'FIRST');
    nqp::bindattr($other, Block, '$!phasers', $table);
    (^2).map($first).List.elems;
    is $fired, 1, 'a phaser table bound on one copy does not reach another';
    (^2).map($other).List.elems;
    is $fired, 1, 'the copy whose table dropped FIRST does not run it';
}

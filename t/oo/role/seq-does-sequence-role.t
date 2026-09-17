use Test;

# `Seq`/`HyperSeq`/`RaceSeq` compose the `Sequence` and `PositionalBindFailover`
# roles in Raku (unlike a plain `Array`/`List`/`Range`/`Slip`, which do
# `Iterable` but NOT `Sequence`/`PositionalBindFailover`). A return-type
# constraint of `Sequence` on a routine returning `.sort`/`.grep`/... (the
# common `method columns() returns Sequence { ... }` shape, seen in the
# `Data::UkraineWar::MoD` distribution) failed every call with
# "Type check failed for return value; expected Sequence but got Seq" because
# `does_role_hierarchy` (src/value/types_isa.rs) had no arm for `Sequence` /
# `PositionalBindFailover` at all, so `Seq ~~ Sequence` was False.

plan 10;

my $seq = (3, 1, 2).sort;
is $seq.WHAT.^name, 'Seq', 'sanity: .sort returns a Seq';

ok $seq ~~ Sequence, 'a Seq value does Sequence';
ok $seq ~~ PositionalBindFailover, 'a Seq value does PositionalBindFailover';
ok $seq ~~ Iterable, 'a Seq value still does Iterable';

ok Seq ~~ Sequence, 'the Seq type object does Sequence';

nok [1, 2, 3] ~~ Sequence, 'an Array does NOT do Sequence';
nok (1, 2, 3) ~~ Sequence, 'a List does NOT do Sequence';
nok [1, 2, 3] ~~ PositionalBindFailover, 'an Array does NOT do PositionalBindFailover';

sub columns() returns Sequence {
    return <b a c>.sort;
}
is-deeply columns.List, ('a', 'b', 'c'), 'a `returns Sequence` routine accepts a Seq return value';

my $hyper = (1..5).hyper.map({ $_ });
ok $hyper ~~ Sequence, 'a HyperSeq value does Sequence too';

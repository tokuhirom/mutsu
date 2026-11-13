use Test;

# HyperSeq and RaceSeq, like Seq, do PositionalBindFailover and not
# Positional; binding one to an `@` or `Positional` parameter caches it into a
# List.

plan 10;

nok (1, 2).hyper ~~ Positional, 'a HyperSeq is not Positional';
nok (1, 2).race ~~ Positional, 'a RaceSeq is not Positional';
nok HyperSeq ~~ Positional, 'the HyperSeq type object is not Positional';
ok (1, 2).hyper ~~ PositionalBindFailover, 'a HyperSeq does PositionalBindFailover';
ok (1, 2).race ~~ Sequence, 'a RaceSeq does Sequence';
ok (1, 2).hyper ~~ Iterable, 'a HyperSeq is Iterable';

sub at(@x) { @x.^name }
sub pd(Positional:D $x) { $x.^name }
is at((1, 2).hyper.map(* + 1)), 'List', 'HyperSeq bound to @x becomes a List';
is pd((1, 2).race.map(* + 1)), 'List', 'RaceSeq bound to Positional:D becomes a List';
is-deeply (sub (@x) { @x })((1, 2, 3).hyper.map(* * 2)), (2, 4, 6), 'the List keeps the values';
is (sub (@x) { @x.elems })((1, 2, 3).race.grep(* > 1)), 2, 'RaceSeq from grep binds too';

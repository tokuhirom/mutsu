use Test;

# `Mu`'s introspection on a hyper or race sequence reads the sequence itself,
# not its `List` (ADR-11276 §9.173), through a variable (the mutating call
# path), a temporary and a run-time method name alike. Every other method
# still reads it as its `List`. Expected values are Rakudo's.

plan 22;

{
    my $h = (1..3).hyper;
    is $h.WHAT.^name, 'HyperSeq', '$h.WHAT';
    is $h.^name, 'HyperSeq', '$h.^name';
    ok $h.isa(HyperSeq), '$h.isa(HyperSeq)';
    nok $h.isa(List), '... and not List';
    ok $h.does(Iterable), '$h.does(Iterable)';
    ok $h.defined, '$h.defined';
    ok $h.DEFINITE, '$h.DEFINITE';
    is $h.WHICH.^name, 'ObjAt', '$h.WHICH';
    my $m = 'isa';
    ok $h."$m"(HyperSeq), 'a run-time method name';
}
{
    my $r = (1..3).race;
    is $r.WHAT.^name, 'RaceSeq', '$r.WHAT';
    is $r.^name, 'RaceSeq', '$r.^name';
    ok $r.isa(RaceSeq), '$r.isa(RaceSeq)';
    nok $r.isa(HyperSeq), '... and not HyperSeq';
}
{
    ok (1..3).hyper.isa(HyperSeq), 'a temporary hyper sequence';
    nok (1..3).hyper.isa(List), '... is not a List';
    is (1..3).race.^name, 'RaceSeq', 'a temporary race sequence';
    ok (1..3).hyper.defined, '... defined';
}

# Any's and List's methods still read the sequence as its List.
{
    is (1..3).hyper.elems, 3, 'elems';
    is (1..3).hyper.map(* * 2).list, (2, 4, 6), 'map';
    is (3, 1, 2).race.sort.list, (1, 2, 3), 'sort';
    nok (1..3).hyper.is-lazy, 'is-lazy';
    is (1..3).hyper.Seq.^name, 'Seq', 'Seq';
}

use Test;

# `List.fmt` formats a `List` element (an `Array`, a `Slip`, itemized or not)
# with the same format and separator, and another `Iterable` element (a `Seq`,
# a `Range`, an associative) with the format and its own default separator
# unless the format is a bare `%s`, which reads the element's `.Str`.

plan 31;

is ((67,),(99,)).fmt("%d", ","), '67,99', Q「((67,),(99,)).fmt("%d", ",")」;
is ((1,2),).fmt("%d"), '1 2', Q「((1,2),).fmt("%d")」;
is ([1,2],[3]).fmt("%d",","), '1,2,3', Q「([1,2],[3]).fmt("%d",",")」;
is ((1,2),3).fmt("<%s>",","), '<1>,<2>,<3>', Q「((1,2),3).fmt("<%s>",",")」;
is ("C".ords,).fmt(Q[\x%x]), '\\x43', Q「("C".ords,).fmt(Q[\x%x])」;
is ($[1,2],3).fmt("<%s>",","), '<1>,<2>,<3>', Q「($[1,2],3).fmt("<%s>",",")」;
is (1..2,3).fmt("<%s>",","), '<1 2>,<3>', Q「(1..2,3).fmt("<%s>",",")」;
is ((1,2).Seq,3).fmt("<%s>",","), '<1 2>,<3>', Q「((1,2).Seq,3).fmt("<%s>",",")」;
is ((1,(2,3)),4).fmt("<%s>",","), '<1>,<2>,<3>,<4>', Q「((1,(2,3)),4).fmt("<%s>",",")」;
is ((),4).fmt("<%s>",","), ',<4>', Q「((),4).fmt("<%s>",",")」;
is (1=>(2,3),).fmt("<%s>",","), "<1\t2 3>", Q「(1=>(2,3),).fmt("<%s>",",")」;
is ((1,2),).fmt, '1 2', Q「((1,2),).fmt」;
is ((1,2),3).fmt("%s"), '1 2 3', Q「((1,2),3).fmt("%s")」;
is ((67,68).Seq,1).fmt("<%x>",","), '<43> <44>,<1>', Q「((67,68).Seq,1).fmt("<%x>",",")」;
is (1..3,).fmt("<%d>",","), '<1> <2> <3>', Q「(1..3,).fmt("<%d>",",")」;
is ((1,2).Seq,).fmt("%5.1f",","), '  1.0   2.0', Q「((1,2).Seq,).fmt("%5.1f",",")」;
is ((1,2).Seq,).fmt("%e"), '1.000000e+00 2.000000e+00', Q「((1,2).Seq,).fmt("%e")」;
is ((1..2),).fmt("%-3d|"), '1  | 2  |', Q「((1..2),).fmt("%-3d|")」;
is ((1,2).Seq,).fmt("%3s|"), '  1|   2|', Q「((1,2).Seq,).fmt("%3s|")」;
is (%(a=>1),).fmt("<%5s>"), '<    a>', Q「(%(a=>1),).fmt("<%5s>")」;
is (%(a=>1),).fmt("<%s>",","), "<a\t1>", Q「(%(a=>1),).fmt("<%s>",",")」;
is ((1,2).Seq,3).fmt("%-s", ","), '1 2,3', Q「((1,2).Seq,3).fmt("%-s", ",")」;
is ((1,(2,3)).Seq,4).fmt("%d", ","), '1 2 3,4', Q「((1,(2,3)).Seq,4).fmt("%d", ",")」;
is (bag(1),).fmt("%d"), '1', Q「(bag(1),).fmt("%d")」;
is ("ab".comb,).fmt("%5s"), '    a     b', Q「("ab".comb,).fmt("%5s")」;
is ((1,2).Seq,3).fmt("%%%d", ","), '%1 %2,%3', Q「((1,2).Seq,3).fmt("%%%d", ",")」;
is ((1,2).Seq,3).fmt("%.1s", ","), '1 2,3', Q「((1,2).Seq,3).fmt("%.1s", ",")」;
is (1..^3,3).fmt("%3d",","), '  1   2,  3', Q「(1..^3,3).fmt("%3d",",")」;
is ((1,2).Slip,3).fmt("%d", ","), '1,2,3', Q「((1,2).Slip,3).fmt("%d", ",")」;
is (("C".ords),("d".ords)).fmt("%x",","), '43,64', Q「(("C".ords),("d".ords)).fmt("%x",",")」;

class A { method Str { "A!" } }
is ((A.new,1),2).fmt("<%s>"), '<A!> <1> <2>', 'a nested element with a user .Str';

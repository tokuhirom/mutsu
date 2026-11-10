use Test;
use nqp;

# nqp::setelems resizes an IterationBuffer in place, as it does an array: 0
# empties it (ParaSeq's "clean slate" of a running limiter), a smaller size
# truncates, a larger one grows it with null objects.
plan 8;

my $o := nqp::create(IterationBuffer);
nqp::push($o, 1); nqp::push($o, 2); nqp::push($o, 3);
is nqp::elems($o), 3, "three pushed";
nqp::setelems($o, 0);
is nqp::elems($o), 0, "setelems 0 empties it";
is $o.List.raku, '()', "and it reads back empty";
nqp::push($o, 7);
is $o.List.raku, '(7,)', "a push after the reset starts over";

my $p := nqp::create(IterationBuffer);
nqp::push($p, 1); nqp::push($p, 2); nqp::push($p, 3);
nqp::setelems($p, 1);
is $p.List.raku, '(1,)', "setelems 1 truncates";
nqp::setelems($p, 3);
is nqp::elems($p), 3, "setelems 3 grows it";
is $p.List[0], 1, "the kept element survives";
is $p.List.raku, '(1, Mu, Mu)', "the grown slots read back as Mu";

use Test;

# A method declared in a nested block of a class or role body closes over that
# block's lexicals. There is one such variable per run of the block, so a write
# made on a worker thread is seen by the main thread and by the other methods
# declared in the same block (#13044).

plan 6;

class Cnt { { my $n = 0; method inc { ++$n } } }
await start { Cnt.inc };
is Cnt.inc, 2, 'a write on a worker reaches the main thread';

class Cnt2 { { my $n = 0; method inc { ++$n } } }
Cnt2.inc;
await start { Cnt2.inc };
is Cnt2.inc, 3, '... also after the main thread wrote first';

class Pair2 { { my $n = 10; method a { ++$n }; method b { $n } } }
Pair2.a;
await start { Pair2.a };
is Pair2.b, 12, 'the methods of one block share the variable across threads';

class Assign { { my $s = "a"; method app { $s ~= "b" } } }
await start { Assign.app };
is Assign.app, 'abb', 'an assignment on a worker is seen too';

role R { { my $n = 0; method inc { ++$n } } }
class RC does R {}
RC.inc;
await start { RC.inc };
is RC.inc, 3, 'a role body\'s nested block lexical is shared';

class Many { { my atomicint $n = 0; method inc { $n⚛++ } } }
await (^4).map: { start { Many.inc for ^25 } };
is Many.inc, 100, 'concurrent atomic increments are not lost';

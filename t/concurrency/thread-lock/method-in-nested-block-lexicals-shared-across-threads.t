use Test;

# A method declared in a nested block of a class or role body closes over that
# block's lexicals. There is one such variable per run of the block, so a write
# made on a worker thread is seen by the main thread and by the other methods
# declared in the same block (#13044).

plan 11;

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

# An `@`/`%` lexical is shared the same way (#13056). It used to be shared only
# until some role was composed anywhere in the program, even an unrelated one
# declared later: the role below is what made these fail.
class Arr { { my @a; method add($x) { @a.push($x) }; method all { @a.join(",") } } }
Arr.add(1);
await start { Arr.add(2) };
is Arr.all, '1,2', 'an array pushed on a worker is seen by the main thread';

class ArrSet { { my @a = 1, 2, 3; method set($i, $v) { @a[$i] = $v }; method all { @a.join(",") } } }
await start { ArrSet.set(1, 20) };
ArrSet.set(2, 30);
is ArrSet.all, '1,20,30', 'an element store on a worker is seen too';

class Hsh { { my %h; method put($k, $v) { %h{$k} = $v }; method keys { %h.keys.sort.join(",") } } }
Hsh.put('a', 1);
await start { Hsh.put('b', 2) };
is Hsh.keys, 'a,b', 'a hash written on a worker is seen by the main thread';

class Many2 { { my @a; my $l = Lock.new; method add($x) { $l.protect: { @a.push($x) } }; method n { @a.elems } } }
await (^4).map: -> $t { start { Many2.add($_) for ^25 } };
is Many2.n, 100, 'locked concurrent pushes are all kept';

class Typed { { my @a; method add($x) { @a.push($x) }; method what { @a.^name } } }
Typed.add(1);
is Typed.what, 'Array', 'the method still sees an Array';

role Unrelated { method x { 1 } }
class UsesRole does Unrelated {}

use Test;
# The native behavior on a container subclass's backing storage is the last
# candidate of a deferral chain, pushed as a `Native` entry by the frame builder
# whenever the receiver VALUE carries storage (ADR-11276 §9.42).

plan 7;
class A is Array { method push(*@a) { callsame } }
my $a = A.new; $a.push(1, 2);
is $a.elems, 2, 'push override on is Array defers to the storage';
class H is Hash { method keys { callsame } }
my $h = H.new; $h<a> = 1;
is $h.keys.join, 'a', 'keys override (not a protocol name) defers to the storage';
role MK is Hash { method keys { callsame } }
my %m is MK; %m<z> = 1;
is %m.keys.join, 'z', 'role punned onto a hash defers to the storage';
class B is BagHash { multi method ASSIGN-KEY(\k, \v) { nextsame } }
my $b = B.new; $b<x> = 3;
is $b<x>, 3, 'multi ASSIGN-KEY on is BagHash defers to the weights';
class W is Array { method elems { callsame } }
is W.new(1, 2, 3).elems, 3, 'elems override defers';
class G is Array { method gist { 'g:' ~ callsame } }
is G.new(1, 2).gist, 'g:[1 2]', 'gist override on is Array defers to the array gist';
class N is Array { method new(|c) { nextsame } }
is N.new(1, 2).elems, 2, 'new override still reaches Mu.new/Array.new';

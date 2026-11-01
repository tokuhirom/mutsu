use v6;
use Test;

# A built-in class as a later parent (`class C is A is Exception`) keeps its
# place in the C3 linearization instead of landing after `Any, Mu`.
# Seen in Net::BGP::Error (`is Net::BGP::Event is Exception`).

plan 6;

class Event { }
class Err is Event is Exception { method message { "x" } }
is Err.^mro.map(*.^name).List, <Err Event Exception Any Mu>, 'is Event is Exception';

class A { }
class B { }
class D is A is Exception { }
is D.^mro.map(*.^name).List, <D A Exception Any Mu>, 'is A is Exception';

class E is A is B is X::AdHoc { }
is E.^mro.map(*.^name).List, <E A B X::AdHoc Exception Any Mu>, 'is A is B is X::AdHoc';

class F is Exception is A { }
is F.^mro.map(*.^name).List, <F Exception A Any Mu>, 'Exception first still works';

ok Err.new ~~ Exception, 'smartmatch against Exception';
is Err.^parents.map(*.^name).List, <Event Exception>, '.^parents';

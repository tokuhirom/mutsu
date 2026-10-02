use Test;

plan 8;

class F is Mu {}
class G is F {}
class H {}
class E is Exception {}

ok F ~~ Mu, 'type object of an `is Mu` class is a Mu';
nok F.new ~~ Any, 'an instance of a class that `is Mu` is not an Any';
nok G.new ~~ Any, 'nor is an instance of its subclass';
ok H.new ~~ Any, 'an ordinary class instance is an Any';
ok E.new ~~ Any, 'an Exception subclass instance is an Any';

sub f($x) { 1 }
dies-ok { f(F.new) }, 'an untyped-$ parameter rejects an `is Mu` instance';
is f(H.new), 1, 'an untyped-$ parameter accepts an ordinary instance';
sub g(Mu $x) { 2 }
is g(F.new), 2, 'a Mu parameter accepts an `is Mu` instance';

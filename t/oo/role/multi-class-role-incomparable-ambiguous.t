use Test;

# A candidate typed with a parent class and one typed with a role the argument's
# class composes have incomparable declared types, so rakudo reports the call as
# ambiguous instead of preferring the nearer one (#14846).
plan 7;

role R {}
class N {}
class B is N does R {}

multi f(N:D $) { 'N' }
multi f(R:D $) { 'R' }
multi f(Mu:D $) { 'mu' }
throws-like { f(B.new) }, X::Multi::Ambiguous, 'a parent class and a composed role tie';

multi g(N:D $) { 'N' }
multi g(R:D $) { 'R' }
multi g(B:D $) { 'B' }
is g(B.new), 'B', 'a candidate on the argument\'s own class is narrower than both';

multi h(N $) { 'N' }
multi h(R $) { 'R' }
is h(N.new), 'N', 'an argument that only the class candidate accepts';
class C does R {}
is h(C.new), 'R', 'an argument that only the role candidate accepts';

role R2 does R {}
class D does R2 {}
multi k(R $) { 'R' }
multi k(R2 $) { 'R2' }
is k(D.new), 'R2', 'two roles on one inheritance chain stay ordered';

multi m(R $) { 'R' }
multi m(N $) { 'N' }
throws-like { m(B.new) }, X::Multi::Ambiguous, 'declaration order does not matter for the tie';

role Q {}
class E does R does Q {}
multi p(R $) { 'R' }
multi p(Q $) { 'Q' }
throws-like { p(E.new) }, X::Multi::Ambiguous, 'two unrelated composed roles tie';

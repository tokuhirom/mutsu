use Test;

# #10083: invoking the Method object `.^lookup` / `.^find_method` returns,
# with an invocant, binds `self` -- a user method reads its attributes, an
# auto-generated accessor returns its value -- and runs exactly the looked-up
# candidate, not a subclass override. Expected values are rakudo's.

plan 9;

class Bar { has $!one = "x"; method one() { $!one }; method two() { "t" } }
my $b = Bar.new;
is Bar.^find_method("two")($b), 't', 'a method without attribute access';
is Bar.^lookup("one")($b), 'x', '.^lookup: a user method reads $!attr';
is Bar.^find_method("one")($b), 'x', '.^find_method: a user method reads $!attr';

class Baz { has $.one = "x" }
is Baz.^lookup("one")(Baz.new), 'x', 'an accessor Method object returns the value';

class A { has $.v = "a"; method m() { "A" ~ $!v } }
class B is A { method m() { "B" } }
is A.^lookup("m")(B.new), 'Aa', 'the looked-up candidate runs on a subclass instance';
my $m = A.^lookup("m");
is B.new.$m(), 'Aa', '$obj.$method form';
my &mm = A.^lookup("m");
is mm(B.new), 'Aa', 'a Method object bound to a &-variable';

class W { has $!w = 3; method add($x, :$y = 0) { $!w + $x + $y } }
is W.^lookup("add")(W.new, 4, :y(1)), 8, 'positional and named arguments pass through';

class Q { has $.q is rw = 1 }
my $q = Q.new;
Q.^lookup("q")($q) = 9;
is $q.q, 9, 'assigning through an is rw accessor Method object';

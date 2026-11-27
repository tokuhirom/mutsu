use Test;

# A method call on an ordinary instance whose (class, method, argument types)
# key the plain-method lane has proven is answered at the front of
# CallMethodMut, before the other lanes (#19232). These calls repeat each
# shape so the second and later calls take that front, and check that what it
# answers is still what the full dispatch answers -- including after the class
# changes under it.

plan 14;

class C {
    has $.a = 7;
    method m($x) { $x * 2 }
    method none() { 'none' }
    method take { 'took' }
}

my $o = C.new;

is (^3).map({ $o.m(3) }).join(','), '6,6,6', 'a planned method, repeated';
is (^3).map({ $o.none }).join(','), 'none,none,none', 'a no-argument method, repeated';
is (^3).map({ $o.a }).join(','), '7,7,7', 'a generated accessor, repeated';
is (^3).map({ $o.m(1.5) }).join(','), '3,3,3', 'another argument type keys apart';
is $o.take, 'took', 'a user method named take';
is $o."m"(5), 10, 'a quoted method name';
is $o.?nope, Nil, '.? of a missing method';
is $o.?m(4), 8, '.? of a present method';
is $o.WHAT.^name, 'C', '.WHAT on the instance';

C.^add_method('late', method { 'late!' });
is (^2).map({ $o.late }).join(','), 'late!,late!', 'a method added at run time';

C.^find_method('m').wrap(-> |c { 'wrapped ' ~ callsame });
is (^2).map({ $o.m(1) }).join(','), 'wrapped 2,wrapped 2', 'a method wrapped after its calls were planned';

class D is C { method m($x) { 'D' ~ $x } }
my $d = D.new;
is (^2).map({ $d.m(1) }).join(','), 'D1,D1', 'a subclass override has a key of its own';
is (^2).map({ $o.m(2) }).join(','), 'wrapped 4,wrapped 4', 'the parent call is unchanged';

my $s = 'abc';
is (^2).map({ $s.chars }).join(','), '3,3', 'a non-instance receiver keeps its own lane';

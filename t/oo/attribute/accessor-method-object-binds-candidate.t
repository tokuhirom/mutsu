use Test;

# A Method object invokes exactly its own candidate: a subclass override of
# the method does not win, and a qualified call runs the candidate's wrap
# chain (#10344).

plan 16;

class D { has $.x is rw; }
class E is D { has $!y = 0; method x is rw { $!y } }

my $e = E.new(x => 1);
is D.^methods.first(*.name eq "x")($e), 1, '.^methods accessor object ignores the override';
is D.^lookup("x")($e), 1, '.^lookup accessor object ignores the override';
is D.^find_method("x")($e), 1, '.^find_method accessor object ignores the override';
is D.^can("x")[0]($e), 1, '.^can accessor object ignores the override';
my $m = D.^lookup("x");
is $e.$m(), 1, '$obj.$method accessor object ignores the override';

D.^lookup("x")($e) = 5;
is $e.D::x, 5, 'assigning through an accessor object writes its own attribute';
is $e.x, 0, 'the override is untouched by that assignment';
$m($e) = 6;
is $e.D::x, 6, 'assigning through a stored accessor object';

class W { has $.x; }
class V is W { method x { 99 } }
my $w = W.^methods.first(*.name eq "x");
$w.wrap(-> $s { "w" ~ callsame });
my $v = V.new(x => 1);
is $v.W::x, 'w1', 'a qualified call runs the accessor wrap chain';
is W.new(x => 3).x, 'w3', 'ordinary dispatch runs the accessor wrap chain';
is $w(W.new(x => 4)), 'w4', 'invoking the wrapped accessor object runs the wrapper';
is $w($v), 'w1', 'the wrapped accessor object binds its candidate on a subclass';

class A::B { has $.x is rw; }
class A::C is A::B { method x { 99 } }
my $c = A::C.new(x => 1);
is A::B.^lookup("x")($c), 1, 'a nested-package owner binds its candidate';
A::B.^lookup("x")($c) = 3;
is $c.A::B::x, 3, 'assigning through a nested-package accessor object';

class P { method m { "P" } }
class Q is P { method m { "Q" } }
is P.^method_table<m>(Q.new), 'P', '.^method_table entry ignores the override';
is P.^lookup("m")(Q.new), 'P', '.^lookup user method object ignores the override';

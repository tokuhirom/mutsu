use Test;
use MONKEY-TYPING;
plan 8;
class A { multi method m(Int $x) { "A-Int" }; multi method m(Any $x) { "A-Any" } }
class B is A { multi method m(Int $x) { "B-Int>" ~ callsame() } }
is B.new.m(1), "B-Int>A-Int", 'callsame defers along the expansion';
is B.new.m(1), "B-Int>A-Int", 'a second call (memoized expansion) agrees';
is B.new.m("s"), "A-Any", 'a non-matching candidate is filtered per call';
augment class A { multi method m(Str $x) { "A-Str" } }
ok B.new.m("s") ~~ Str, 'a call after an augment still dispatches';
is B.new.m(1), "B-Int>A-Int", 'existing chain unchanged by the augment';
class C is B { multi method m(Int $x) { "C>" ~ callsame() } }
is C.new.m(2), "C>B-Int>A-Int", 'a new subclass builds its own expansion';
is B.new.m(2), "B-Int>A-Int", 'the parent is unaffected';
class D { method one { "d" } }
class E is D { method one { "e>" ~ callsame() } }
is E.new.one, "e>d", 'a plain method chain still defers';

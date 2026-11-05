use v6;
use Test;
use MONKEY-TYPING;

# Rakudo builds a subclass's multi dispatcher from its parent's when the
# subclass is composed, so a candidate an `augment` adds to the parent
# afterwards does not join it (#16049).
plan 10;

class A {
    multi method m(Int $x) { "A-Int" }
    multi method m(Any $x) { "A-Any" }
}
class B is A { multi method m(Int $x) { "B-Int>" ~ callsame() } }
class C is B { multi method m(Num $x) { "C-Num>" ~ callsame() } }
class D is A { }

is B.new.m("s"), "A-Any", "before the augment";

augment class A { multi method m(Str $x) { "A-Str" } }

is A.new.m("s"), "A-Str", "the augmented class itself sees the candidate";
is B.new.m("s"), "A-Any", "a subclass with its own multi dispatcher does not";
is C.new.m("s"), "A-Any", "nor does its subclass";
is B.new.m(1), "B-Int>A-Int", "the inherited chain is unchanged";
is D.new.m("s"), "A-Str", "a subclass without its own candidates inherits the parent's dispatcher";

augment class A { method plain() { "plain" } }
is B.new.plain, "plain", "an augmented single method is found by lookup";

augment class A { multi method m(Rat $x) { "A-Rat" } }
is D.new.m(1.5), "A-Rat", "a later augment reaches the subclass without its own dispatcher";

# A subclass declared AFTER the augment composes after it, so it sees the
# candidate (#16314): declaration order, not run-time registration order.
class E is A { multi method m(Rat $x) { "E-Rat" } }
is E.new.m("s"), "A-Str", "a subclass declared after the augment sees the augmented candidate";
is E.new.m(1.5), "E-Rat", "and keeps its own candidate";

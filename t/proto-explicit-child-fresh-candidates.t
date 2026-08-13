use Test;

# ADR-0019 E9-pre ticket (verified against Rakudo v2026.06, 2026-08-13):
# an EXPLICIT `proto method` declared in a subclass starts a FRESH candidate
# set. Its `{*}` redispatch only sees multi candidates declared at or below
# its own declaring class in the MRO — a parent's own multi candidates of the
# same name are NOT reachable through it, even though those parent candidates
# ARE reachable when the child has no proto of its own (the implicit case,
# pinned by t/proto-star-cross-mro-candidates.t: a proto in a PARENT governs
# candidates a child adds).
# See todo/tickets/explicit-child-proto-assumes-parent-candidates.md (now
# resolved).

plan 4;

class P { multi method m(Int $x) { "p-int" } }
class C is P {
    proto method m($x) { {*} }
    multi method m(Str $x) { "c-str" }
}
throws-like { C.new.m(5) }, X::Multi::NoMatch,
    "explicit child proto does not see the parent's Int candidate";
is C.new.m("hi"), "c-str",
    "explicit child proto still dispatches its own candidates";

# The boundary tracks the class that actually declared the governing proto,
# not just "the receiver's own class": a proto declared on a MIDDLE class of
# the MRO still excludes candidates from classes above it, while a class
# below that middle class with no proto of its own keeps inheriting through
# it transitively (mirroring the parent-governs-child pin, just one level
# deeper).
class A {
    proto method q($x) { {*} }
    multi method q(Int $x) { "a-int" }
}
class B is A {
    proto method q($x) { {*} }
    multi method q(Str $x) { "b-str" }
}
class D is B { multi method q(Num $x) { "d-num" } }
is D.new.q("s"), "b-str",
    "a mid-MRO explicit proto governs a further-derived class with no proto of its own";
throws-like { D.new.q(1) }, X::Multi::NoMatch,
    "the mid-MRO explicit proto still excludes the ancestor's Int candidate";

use Test;
plan 7;

# A subclass's `multi method` gets an implicit proto over multis only: the
# parent's `only` method is shadowed, not a further candidate (#17572).
class Parent { method m($x) { 'parent' } }
class Child is Parent { multi method m(Int $x where * > 10) { 'child' } }

is Child.m(11), 'child', 'the matching child multi wins';
throws-like { Child.m(1) }, X::Multi::NoMatch,
    'a failed child multi does not fall back to the parent only (where)';
throws-like { Child.m('s') }, X::Multi::NoMatch,
    'a failed child multi does not fall back to the parent only (type)';
is Parent.m(1), 'parent', 'the parent itself still dispatches to its only';

# A parent multi is a further candidate of the child's proto.
class MP { multi method n(Str $x) { 'mp-str' } }
class MC is MP { multi method n(Int $x) { 'mc-int' } }
is MC.n(1), 'mc-int', 'child multi';
is MC.n('a'), 'mp-str', 'parent multi stays a candidate';

# An ancestor multi past a parent only is still a candidate: the only is
# skipped, never the walk ended.
class G { multi method k(Str $x) { 'g' } }
class P2 is G { method k($x) { 'p2' } }
class C2 is P2 { multi method k(Int $x) { 'c2' } }
is C2.k("s"), "g", "the only in between is skipped";

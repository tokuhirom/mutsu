use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# a proto declared in a PARENT governs multi candidates added by a child class
# (the child's implicit proto clones the parent's), and nextsame walks between
# candidates under an explicit proto.

plan 3;

my @ev;
class P2 {
    proto method n($x) { @ev.push("proto({$x})"); my $r = {*}; "P-$r" }
    multi method n(Int $x) { @ev.push("Int"); "p2-int" }
}
class C2 is P2 { multi method n(Str $x) { @ev.push("Str"); "c2-str" } }
is C2.new.n(1), "P-p2-int", "parent proto body governs the parent candidate";
is C2.new.n("a"), "P-c2-str", "parent proto body governs a child-added candidate";

class D {
    proto method p($x) { {*} }
    multi method p(Int $x) { nextsame }
    multi method p(Any $x) { "d-any" }
}
is D.new.p(3), "d-any", "nextsame between candidates under an explicit proto";

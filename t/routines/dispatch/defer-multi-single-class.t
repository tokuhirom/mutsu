use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# nextsame/callsame walk multi candidates of one class narrowest-first, code after
# nextsame never runs, and an exhausted chain yields Nil.

plan 4;

my @ev;
class A {
    multi method m(Int $x) { @ev.push("Int($x)"); nextsame; @ev.push("Int-after") }
    multi method m(Cool $x) { @ev.push("Cool($x)"); nextsame; @ev.push("Cool-after") }
    multi method m(Any $x) { @ev.push("Any($x)") }
}
A.new.m(42);
is @ev.join("|"), "Int(42)|Cool(42)|Any(42)",
    "nextsame walks Int -> Cool -> Any; code after nextsame never runs";

class B {
    multi method m(Int $x) { my $r = callsame; "int:" ~ ($r // "Nil") }
    multi method m(Any $x) { "b-any" }
}
is B.new.m(1), "int:b-any", "callsame returns the next candidate's value";

my @ev2;
class C2 {
    multi method m(Int $x) { @ev2.push("Int"); nextsame; @ev2.push("after") }
}
my $r = C2.new.m(7);
ok !$r.defined, "nextsame with no next candidate returns Nil";
is @ev2.join("|"), "Int", "code after a no-next nextsame does not run";

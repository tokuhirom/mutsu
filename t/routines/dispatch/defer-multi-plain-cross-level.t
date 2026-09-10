use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# a multi candidate in the child defers to a PLAIN same-name method in the parent,
# and a plain child method defers into the parent's multi set (narrowest wins).
# (The both-levels-multi ordering case deliberately has NO pin here: mutsu still
# diverges from raku there — see todo/deep/defer-chain-ranked-multi-order.md.)

plan 4;

my @ev;
class P { method m($x) { @ev.push("P"); "p" } }
class C is P { multi method m(Int $x) { @ev.push("C:Int"); my $r = callsame; "c-$r" } }
is C.new.m(1), "c-p", "multi child defers to the plain parent method";
is @ev.join("|"), "C:Int|P", "multi-child -> plain-parent order";

my @ev2;
class P2 {
    multi method m(Int $x) { @ev2.push("P2:Int"); "p2-int" }
    multi method m(Any $x) { @ev2.push("P2:Any"); "p2-any" }
}
class C2 is P2 { method m($x) { @ev2.push("C2"); my $r = callsame; "c2-$r" } }
is C2.new.m(1), "c2-p2-int", "plain child defers into the parent's multi set";
is @ev2.join("|"), "C2|P2:Int", "narrowest parent candidate is chosen by the deferral";

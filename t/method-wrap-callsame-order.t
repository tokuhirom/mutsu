use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# callsame inside a method wrapper reaches the original; the most recently applied
# wrapper is outermost.

plan 3;

my @ev;
class C { method m($x) { @ev.push("orig($x)"); "orig" } }
C.^lookup('m').wrap(-> |c { @ev.push("w1-in"); my $r = callsame; @ev.push("w1-out($r)"); "w1-$r" });
is C.new.m(5), "w1-orig", "wrapper composes its result with the original's";

C.^lookup('m').wrap(-> |c { @ev.push("w2-in"); my $r = callsame; @ev.push("w2-out($r)"); "w2-$r" });
@ev = ();
is C.new.m(6), "w2-w1-orig", "most recently applied wrapper runs outermost";
is @ev.join("|"), "w2-in|w1-in|orig(6)|w1-out(orig)|w2-out(w1-orig)",
    "double-wrap event order";

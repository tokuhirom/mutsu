use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# a wrap applied mid-MRO (on the parent's method) is entered when the child's
# callsame walks up; a wrap on the child wraps only the child's own entry.

plan 3;

my @ev;
class P { method m() { @ev.push("P"); "p" } }
class C is P { method m() { @ev.push("C"); my $r = callsame; "c-$r" } }
P.^lookup('m').wrap(-> |c { @ev.push("Pw-in"); my $r = callsame; "pw-$r" });
is C.new.m, "c-pw-p", "callsame from the child reaches the parent's wrapper first";
is @ev.join("|"), "C|Pw-in|P", "mid-MRO wrap event order";

class P2 { method m() { "p2" } }
class C2 is P2 { method m() { my $r = callsame; "c2-$r" } }
C2.^lookup('m').wrap(-> |c { my $r = callsame; "cw-$r" });
is C2.new.m, "cw-c2-p2", "wrap on child: wrapper -> child body -> callsame to parent";

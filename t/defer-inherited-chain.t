use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# callsame/nextsame walk a plain-method inheritance chain in MRO order; the top of
# the user chain (no same-name parent method) yields Nil.

plan 4;

my @ev;
class GP { method m() { @ev.push("GP"); "gp" } }
class P is GP { method m() { @ev.push("P"); my $r = callsame; @ev.push("P-got($r)"); "p" } }
class C is P { method m() { @ev.push("C"); my $r = callsame; @ev.push("C-got($r)"); "c" } }
is C.new.m, "c", "3-level callsame returns receiver's own result";
is @ev.join("|"), "C|P|GP|P-got(gp)|C-got(p)", "callsame chain order C -> P -> GP";

class C3 is GP { method m() { nextsame } }
is C3.new.m, "gp", "nextsame passes the parent's return value through";

class Solo { method only() { my $r = callsame; $r // "was-nil" } }
is Solo.new.only, "was-nil", "callsame at the top of the chain returns Nil";

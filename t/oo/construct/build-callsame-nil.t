use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# BUILDALL runs BUILD submethods parent-first; callsame inside a BUILD submethod
# finds no next candidate (submethods do not inherit) and returns Nil.

plan 1;

my @ev;
class P { submethod BUILD() { @ev.push("P") } }
class C is P { submethod BUILD() { @ev.push("C"); my $r = callsame; @ev.push("got({$r // 'Nil'})") } }
C.new;
is @ev.join("|"), "P|C|got(Nil)", "parent-first BUILD order; callsame in BUILD yields Nil";

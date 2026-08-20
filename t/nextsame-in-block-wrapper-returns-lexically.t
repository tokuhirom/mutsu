use Test;

# Pin for todo/tickets/nextsame-tail-call-is-not-a-real-return-signal.md
# (verified against Rakudo v2026.06, 2026-08-20):
#
# `nextsame`'s tail-call unwind must raise a real `CX::Return` signal (with
# `control: Some(Control::Return)`), not a hand-built targetless error. That is
# what lets a `nextsame` inside a non-Routine `.wrap()` block find its
# *lexically enclosing* Routine as the return target, unwinding all the way
# out of the sub the wrapper was written inside -- rather than being absorbed
# by the nearest dynamic frame (the method call the wrapper block wraps).

plan 3;

# Probe 1 (tmp/v1): bare `nextsame` in tail position inside a non-Routine
# `.wrap()` block. `nextsame` must unwind clear out of `run1`, abandoning the
# `@ev.push('unreached')` statement after the `.m` call.
my @ev1;
sub run1() {
    class C1 { method m() { @ev1.push("orig"); "o" } }
    C1.^lookup('m').wrap(-> |c { @ev1.push("wrap"); nextsame; @ev1.push("wrap-unreached") });
    @ev1.push(C1.new.m);
    @ev1.push("unreached");
}
run1();
is @ev1.join("|"), "wrap|orig", "bare nextsame in a block wrapper returns lexically, skipping the caller's statement";

# Probe 2 (tmp/v8): `nextsame` in NON-tail position (`my $x = nextsame`).
# Raku still unwinds all the way out -- this is not about syntactic tail
# position, it is about the callee (the wrapper block) not being a Routine.
my @ev8;
sub run8() {
    class C8 { method m() { @ev8.push("orig"); "o" } }
    C8.^lookup('m').wrap(-> |c { @ev8.push("wrap"); my $x = nextsame; @ev8.push("wrap-unreached") });
    @ev8.push(C8.new.m);
    @ev8.push("unreached");
}
run8();
is @ev8.join("|"), "wrap|orig", "non-tail-position nextsame in a block wrapper still returns lexically";

# Probe 3 (tmp/v14) -- ANTI-REGRESSION: the wrapper written as `sub (|c) { ... }`
# IS a Routine, so `nextsame` returns from the wrapper itself instead of
# unwinding further. `C14.new.m` legitimately yields "o" here, and the
# statement after `nextsame` inside the wrapper must NOT run. This case
# already behaved correctly before the fix and must keep behaving correctly.
my @ev14;
sub run14() {
    class C14 { method m() { @ev14.push("orig"); "o" } }
    C14.^lookup('m').wrap(sub (|c) { @ev14.push("wrap"); nextsame; @ev14.push("wrap-unreached") });
    @ev14.push(C14.new.m);
    @ev14.push("after-call");
}
run14();
is @ev14.join("|"), "wrap|orig|o|after-call", "a Routine wrapper's nextsame returns from the wrapper itself, not the caller";

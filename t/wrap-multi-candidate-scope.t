use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# wrapping one multi candidate (via .^lookup(...).candidates) affects only that
# candidate's dispatch; sibling candidates are untouched. Candidate order is
# declaration order.

plan 2;

class C {
    multi method m(Int $x) { "int" }
    multi method m(Str $x) { "str" }
}
C.^lookup('m').candidates[0].wrap(-> |c { my $r = callsame; "w-$r" });
is C.new.m(1), "w-int", "wrapping candidate 0 (Int) wraps Int dispatch";
is C.new.m("a"), "str", "sibling candidate is unaffected by the wrap";

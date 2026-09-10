use Test;

# ADR-0019 E9c design pin (verified against Rakudo v2026.06, 2026-08-13):
# `samewith` re-runs the governing PROTO BODY, not just the sequence's ranked
# candidate list -- a full dispatcher restart, not a same-sequence re-rank.
# See probe P3 in
# todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md ("E9c design
# (2026-08-13): proto {*} resolves directly within the governing boundary;
# samewith's by-name restart is CONFIRMED correct and stays").

plan 2;

my @ev;
class C {
    proto method m($x) { @ev.push("proto($x)"); {*} }
    multi method m(Int $x) { @ev.push("int($x)"); samewith($x + 10) if $x < 10; }
    multi method m(Str $s) { @ev.push("str($s)") }
}
C.new.m(1);
is @ev.join("|"), "proto(1)|int(1)|proto(11)|int(11)",
    "samewith re-runs the governing proto method body (method case)";

@ev = ();
proto sub f($x) { @ev.push("proto($x)"); {*} }
multi sub f(Int $x) { @ev.push("int($x)"); samewith("s") if $x != 0; }
multi sub f(Str $s) { @ev.push("str($s)") }
f(1);
is @ev.join("|"), "proto(1)|int(1)|proto(s)|str(s)",
    "samewith re-runs the governing proto sub body (sub case)";

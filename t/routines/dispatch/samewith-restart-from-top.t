use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# samewith re-runs the dispatch from the top with new args — including when called
# from a candidate that was itself reached via nextsame.

plan 3;

class C {
    multi method m(Int $x) { my $r = samewith("hello"); "int-$r" }
    multi method m(Str $x) { "str" }
}
is C.new.m(42), "int-str", "samewith with new args restarts dispatch from the top";

my @ev;
class D {
    multi method m(Int $x) { @ev.push("Int"); nextsame }
    multi method m(Any $x) { @ev.push("Any({$x.^name})"); $x ~~ Int ?? samewith("s") !! "any-done" }
}
is D.new.m(3), "any-done", "samewith from a nextsame-reached candidate restarts at the top";
is @ev.join("|"), "Int|Any(Int)|Any(Str)", "restart re-enters the ranked list from the top";

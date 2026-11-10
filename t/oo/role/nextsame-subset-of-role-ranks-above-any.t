use Test;

# From the Red distribution (Red::AST::Optimizer::AND): `nextsame` out of a
# multi candidate must defer to a candidate whose parameters are a subset of a
# *role* before the all-`$` catch-all candidate, because a role constraint is
# narrower than an unconstrained (Any) parameter.

plan 4;

role Infix { has $.left }
class AST does Infix { }
class Ge is AST { }
class Gt is AST { }
my subset GeGt of Infix where Ge|Gt;

my @trace;
role R {
    multi method opt(Ge, Ge, 1) { @trace.push("C1"); nextsame }
    multi method opt(GeGt $l, GeGt $r, $) { @trace.push("C2"); "second" }
    multi method opt($, $, $) { @trace.push("C3"); "catch-all" }
}
class K does R { }

my $x = Ge.new;
is K.opt($x, $x, 2), "second", "direct dispatch picks the subset-of-role candidate";
is-deeply @trace, ["C2"], "only C2 ran";

@trace = ();
is K.opt($x, $x, 1), "second", "nextsame defers to the subset-of-role candidate, not the catch-all";
is-deeply @trace, ["C1", "C2"], "deferral order C1 then C2";

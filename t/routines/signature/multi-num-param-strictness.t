use Test;

# ADR-0019 E9-pre follow-up pin (verified against Rakudo v2026.06, 2026-08-12):
# a nominal `Num` parameter does NOT match Int/Rat arguments in multi dispatch
# (Num means floating point; 1 !~~ Num). raku rejects at dispatch with
# X::Multi::NoMatch. Numeric/Real/Cool keep matching, and Num() coercion keeps
# accepting Int. The old behavior (matcher admitted Int, binder then died with
# X::TypeCheck::Binding::Parameter, sub-side even bound silently) was a
# mutsu-only "numeric widening" — see
# news/2026-08/multi-num-param-strictness.md.

plan 11;

class T {
    multi method m(Num $x)      { "Num" }
    multi method nu(Numeric $x) { "Numeric" }
    multi method re(Real $x)    { "Real" }
    multi method co(Num() $x)   { "coerced-{$x.^name}" }
}
my $t = T.new;

throws-like { $t.m(1) }, X::Multi::NoMatch, "Int arg does not match a Num multi param";
throws-like { $t.m(1.5) }, X::Multi::NoMatch, "Rat arg does not match a Num multi param";
is $t.m(1e0), "Num", "a real Num arg still matches";
is $t.nu(1), "Numeric", "Numeric still matches Int";
is $t.re(1), "Real", "Real still matches Int";
is $t.co(1), "coerced-Num", "Num() coercion still accepts Int";

class U {
    multi method m(Num $x) { "U:Num" }
    multi method m(Any $x) { "U:Any" }
}
is U.new.m(1), "U:Any", "Int arg picks the Any sibling, not the Num candidate";
is U.new.m(1e0), "U:Num", "Num arg picks the Num candidate";

multi sub f(Num $x) { "sub:Num" }
multi sub f(Str $x) { "sub:Str" }
# a variable argument defeats rakudo's compile-time reachability check, so the
# rejection is exercised at runtime dispatch in both implementations
my $one = 1;
throws-like { f($one) }, X::Multi::NoMatch, "sub multi: Int arg does not match Num param";
is f(2e0), "sub:Num", "sub multi: Num arg matches";

# deferral advance uses the same strict filter: a Num candidate in the chain is
# skipped for an Int argument instead of being invoked and dying in the binder
# (E9-pre model probe 2 shape).
my @ev;
class A2 { multi method m(Int $x) { @ev.push("A:Int"); nextsame; @ev.push("A:u") } }
class B2 is A2 { multi method m(Str $x) { @ev.push("B:Str"); "b" } }
class C2 is B2 { multi method m(Num $x) { @ev.push("C:Num"); "c" } }
my $r = C2.new.m(1);
ok !$r.defined && !@ev.grep("C:Num") && !@ev.grep("B:Str"),
    "nextsame skips non-matching Num/Str candidates instead of dying in the binder";

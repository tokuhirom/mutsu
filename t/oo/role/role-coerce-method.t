use v6;
use Test;

# A coercion type whose target is a role (`R()`) calls the role's own
# `COERCE` on the role's pun, as PDF's `role PDF::IO` does with a
# `proto method COERCE` and one `multi` per source type (#12907). Every
# expectation is Rakudo v2026.09's output.

plan 8;

role R { method COERCE(Str:D $v) { "s:$v" but R } }
sub f(R() $x) { $x }
is f("a"), 's:a', 'a role COERCE method coerces a parameter';
ok f("a") ~~ R, 'the result does the role';

role P {
    proto method COERCE($v) returns P {*}
    multi method COERCE(Str:D $v) { "p:$v" but P }
    multi method COERCE(Int:D $v) { self.COERCE(~$v) }
}
sub g(P() $x) { $x }
is g("a"), 'p:a', 'a proto/multi COERCE';
is g(42), 'p:42', 'a multi candidate that redispatches through self.COERCE';
my P() $y = "b";
is $y, 'p:b', 'a coercing variable';

role M { multi method COERCE(Str:D $v) { "m:$v" but M } }
sub h(M() $x) { $x }
throws-like { h(42) }, X::Coerce::Impossible,
    'no candidate accepts the value: X::Coerce::Impossible';

role W { method COERCE($v) { 42 } }
sub w(W() $x) { $x }
throws-like { w(4) }, X::Coerce::Impossible,
    'a COERCE result that does not do the role is an impossible coercion';

class C { proto method COERCE($v) {*}; multi method COERCE(Str:D $v) { C.new } }
sub c(C() $x) { $x }
isa-ok c("a"), C, 'a class proto/multi COERCE still works';

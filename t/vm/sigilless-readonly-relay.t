use Test;

# A sigilless parameter passed on to another method's sigilless parameter.
# When the caller's parameter is bound to a value it is read-only, so the
# callee's is too; when it aliases a variable, the callee aliases that same
# variable and can assign through it.

plan 7;

class C {
    method w(\x) { x = 5 }
    method relay(\y) { self.w(y) }
    method relay2(Mu \y) { self.r(y) }
    method r(Mu \z) { z.^name }
    method relay3(Mu \y) { self.relay2(y) }
}

my $v = 1;
C.relay($v);
is $v, 5, 'a relayed alias of a variable still assigns through to it';

throws-like { C.relay(1) }, X::Assignment::RO, 'a relayed value stays read-only';
is C.relay2(Int), 'Int', 'a type object relays through a Mu sigilless parameter';
my $u = Str;
is C.relay2($u), 'Str', 'a variable relays through a Mu sigilless parameter';
is C.relay3(Rat), 'Rat', 'two relays deep';

my \t = 42;
throws-like { C.w(t) }, X::Assignment::RO, 'a sigilless term is read-only in the callee';
is t, 42, 'and keeps its value';

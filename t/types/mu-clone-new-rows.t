use Test;

# `clone` and `new` are declared on `Mu`, so a user override that defers ends at
# them: the attribute-copying clone with its `:attr(v)` twiddles, and
# `Mu.new(*%attrinit)` (ADR-11276 §9.48).

plan 8;

class P {
    has $.a = 1;
    has $.b = 2;
    method clone(|c) { callsame }
}
my $p = P.new(a => 10);
my $q = $p.clone(b => 20);
is "{$q.a} {$q.b} {$p.b}", '10 20 2', 'callsame from a clone override applies the twiddles to a copy';
ok $p !=== $q, '... and the copy is a distinct object';

class Q {
    has $.n;
    method new(|c) { nextsame }
}
is Q.new(n => 3).n, 3, 'nextsame from a new override reaches Mu.new';

class R {
    has $.n;
    method new(|c) { nextwith(|c, n => 9) }
}
is R.new.n, 9, 'nextwith passes replacement named arguments to Mu.new';

class S {
    has $.n;
    method new(|c) { nextsame }
}
throws-like { S.new(5) }, X::Constructor::Positional, 'Mu.new refuses positional arguments';

class V is Version {
    method new(|c) { nextsame }
}
is V.new('1.2.3').gist, 'v1.2.3', 'the nearest builtin ancestor constructor comes before bless';

role Rl {
    has $.m;
    method new(|c) { nextsame }
}
class W does Rl { }
is W.new(m => 4).m, 4, 'a role-provided new defers the same way';

class X {
    has $.k = 5;
    method clone(|c) { nextsame }
}
is X.new.clone(k => 6).k, 6, 'nextsame from a clone override reaches the native clone';

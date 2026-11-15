use Test;

# `Mu.clone` on a user-class instance: a shallow copy with named twiddles
# applied to public attributes, unless the class or a role declares `clone`.

plan 18;

class P {
    has $.a = 1; has @.l; has %.h; has Int $.n is rw = 0; has $!priv = 'p';
    method priv { $!priv }
}
my $p = P.new(:l(1, 2), :h(x => 1));
my $c = $p.clone;
is $c.a, 1, 'scalar attribute copied';
is-deeply $c.l.List, (1, 2), 'array attribute copied';
is-deeply $c.h, %(x => 1), 'hash attribute copied';
is $c.priv, 'p', 'private attribute copied';
nok $c === $p, 'a new object';
$c.l.push: 3;
is-deeply $p.l.List, (1, 2, 3), 'the clone is shallow';

my $d = $p.clone(:a(5), :l([9]), :n(3));
is $d.a, 5, 'scalar twiddle';
is-deeply $d.l.List, (9,), 'array twiddle';
is $d.n, 3, 'typed twiddle';
is $p.a, 1, 'the original is untouched';
throws-like { $p.clone(:n('x')) }, X::TypeCheck::Assignment, 'a typed twiddle is checked';
is P.new.clone(:priv<z>).priv, 'p', 'a private attribute is not twiddled';

class U { has $.v; method clone { callsame } }
is U.new(:v(1)).clone(:v(2)).v, 2, 'a user clone deferring to Mu.clone';
role CR { method clone(*%) { 'role clone' } }
class W does CR { has $.v }
is W.new.clone, 'role clone', 'a role clone wins';
class E is Exception { has $.m; method message { $!m } }
is E.new(:m<boom>).clone(:m<bang>).message, 'bang', 'an Exception subclass';

class Inner { has $.value = 7 }
class Delegating { has $.inner handles <clone> }
is Delegating.new(inner => Inner.new).clone.WHAT.^name, 'Inner',
    'a delegated clone wins over Mu.clone';

is (P.new but role { method clone(*%) { 'mixed-in clone' } }).clone,
    'mixed-in clone', 'a mixed-in clone wins over Mu.clone';

class Unbuilt { has $.value is built(False) = 1 }
is Unbuilt.new.clone(:value(5)).value, 5,
    'clone twiddles a public attribute even when new cannot build it';

use Test;

# A user class that composes `Real` gets the role's methods, which compute
# through its `Bridge` (ADR-11276 §9.179); a class that does not compose it is
# not bridged, whatever methods it has.

plan 29;

class R does Real {
    has $.n;
    method Bridge { $!n.Num }
}
my $r = R.new(n => -2.5);

is $r.Int, -2, 'Int bridges';
is $r.floor, -3, 'floor bridges';
is $r.round, -2, 'round bridges';
is $r.sqrt.isNaN, True, 'sqrt bridges';
is $r.Str, '-2.5', 'Str bridges';
is $r.gist, '-2.5', 'gist is the Str';
is $r ~ '!', '-2.5!', 'infix ~ stringifies through Real.Str';
ok $r eq '-2.5', 'infix eq stringifies through Real.Str';
is $r.abs, 2.5, 'abs of a negative value';
isa-ok R.new(n => 4).abs, R, 'abs of a non-negative value is the invocant';
isa-ok $r.Real, R, 'Real is the invocant';
isa-ok $r.Numeric, R, 'Numeric is the invocant';
isa-ok $r.conj, R, 'conj is the invocant';
is $r.isNaN, False, 'isNaN is False';
is $r.succ, -1.5, 'succ is self + 1';
is $r.pred, -3.5, 'pred is self - 1';
ok $r.Bool, 'Bool of a non-zero value';
nok R.new(n => 0).Bool, 'Bool of zero';
is R.new(n => 6.25).log(2), 2.643856189774725e0, 'log($base) bridges';
is R.new(n => 6.25).base(2), '110.01', 'base bridges';

class Own does Real {
    method Bridge { 9e0 }
    multi method sqrt(Own:D:) { 'own' }
}
is Own.new.sqrt, 'own', "the class's own method wins over the role's";

class B { method Bridge { 2e0 } }
throws-like { B.new.log(2) }, X::Method::NotFound, 'a Bridge alone does not make a class Real';
ok B.new.Bool, 'a class with a Bridge keeps Mu.Bool';

class M { method Numeric { 9 } }
throws-like { M.new.sqrt }, X::Method::NotFound, 'a Numeric method alone does not either';
is M.new + 1, 10, 'but the operators numify through it';

# The type object's numeric coercions are the roles' `:U` candidates: they
# warn about the undefined value and answer `self.new`.
class Z does Real { has $.n = 0; method Bridge { $!n.Num } }
isa-ok (quietly Z.Numeric), Z, 'Numeric of the type object is a new instance';
isa-ok (quietly Z.Real), Z, 'Real of the type object too';
class NumOnly does Numeric { }
isa-ok (quietly NumOnly.Numeric), NumOnly, 'and Numeric of a Numeric-only class';
my $warned = 0;
{ Z.Numeric; CONTROL { when CX::Warn { $warned++; .resume } } }
is $warned, 1, 'the coercion warns';

use Test;

# A package-qualified multi call (`M::a(...)`) ranks every candidate of the
# name, in every arity, in one set of rakudo topological tiers, as a bare-name
# call does (#12795). It used to try the exact-arity candidates first, so a
# narrower candidate with an optional parameter lost to an exact-arity one.
# Expected values from rakudo.

plan 12;

sub t(&c) { my $r = try c(); $r // ($! ?? $!.^name !! 'Nil') }

module M {
    our proto a(|) {*}
    multi a(Cool $x)            { 'exact-Cool' }
    multi a(Int $x, $y?)        { 'opt-Int' }

    our proto c(|) {*}
    multi c(Int $a?)            { 'Int' }
    multi c()                   { 'zero' }

    our proto e(|) {*}
    multi e($n)                 { 'plain' }
    multi e(|c($x))             { 'capture' }

    our proto f(|) {*}
    multi f(*@a)                { 'slurpy' }
    multi f(|c($x))             { 'capture' }

    our proto g(|) {*}
    multi g(Numeric $x, Numeric $y) { 'two' }
    multi g(Int $x, *@r)            { 'slurpy' }
}

is M::a(1),   'opt-Int',    'a narrower optional-arity candidate beats an exact-arity one';
is M::a('s'), 'exact-Cool', 'the exact-arity one still wins what only it binds';
is M::c(),    'zero',       'the zero-arity candidate wins a call with no arguments';
is M::c(5),   'Int',        'the optional one takes an argument';
is M::e(1),   'plain',      'a plain positional beats a sub-signature capture';
is t({ M::e(1, 2) }), 'X::Multi::NoMatch', 'the capture still checks its sub-signature';
is M::f(1),   'capture',    'a sub-signature capture beats a slurpy array';
is M::g(1, 2), 'two',       'a fixed-arity candidate beats a slurpy one of another arity';
is M::g(1),   'slurpy',     'the slurpy one takes the call only it binds';

# Through a stored routine object, which reaches the proto's candidates.
my &a = &M::a;
is a(1), 'opt-Int', 'the same through a routine object';

package P {
    our sub q(Int $x, $y = 1) { 'our-sub' }
}
is P::q(1), 'our-sub', 'a qualified only-sub with a default still resolves';
is P::q(1, 2), 'our-sub', 'and with the defaulted argument passed';

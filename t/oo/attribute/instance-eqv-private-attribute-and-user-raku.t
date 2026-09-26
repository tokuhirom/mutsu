use Test;

# Rakudo's `infix:<eqv>(Any:D, Any:D)`: two objects are eqv when they are the
# same object, or have the same .WHAT and equal .raku strings (issue #9591).

plan 18;

class A { has $.x; has $!y; method set { $!y = 5 } }
my $a = A.new(x => 1);
my $b = A.new(x => 1);
$b.set;
ok $a eqv $b, 'a private-attribute difference is invisible to eqv';
nok A.new(x => 1) eqv A.new(x => 2), 'a public-attribute difference still counts';
ok [$a] eqv [$b], 'the private-attribute rule applies to nested instances';
ok { k => $a } eqv { k => $b }, '... and to instances inside a hash';

class B { has $.x; method raku { "B" } }
ok B.new(x => 1) eqv B.new(x => 2), 'a user raku decides eqv';
ok [B.new(x => 1)] eqv [B.new(x => 2)], '... also for nested instances';

class C { has $.x; method raku { "C.new(x => {$!x % 2})" } }
ok C.new(x => 1) eqv C.new(x => 3), 'equal user raku strings are eqv';
nok C.new(x => 1) eqv C.new(x => 2), 'different user raku strings are not eqv';

class D is B { }
ok D.new(x => 1) eqv D.new(x => 2), 'an inherited user raku decides eqv';
nok B.new(x => 1) eqv D.new(x => 1), 'different .WHAT is never eqv';

role R does Positional { has $.x; method raku { "R" } }
ok R.new(x => 1) eqv R.new(x => 2), 'a punned role with a user raku';

class E { has $.x; method raku { die "boom" } }
throws-like { E.new(x => 1) eqv E.new(x => 2) }, X::AdHoc, message => 'boom',
    'an exception from a user raku propagates out of eqv';
my $e = E.new(x => 1);
ok $e eqv $e, 'the same object is eqv without calling raku';

is-deeply B.new(x => 1), B.new(x => 2), 'is-deeply uses the same rule';

role S { has $.x; has $!y; method sety { $!y = 3 }; method m { } }
my $p = S.new(x => 1);
my $q = S.new(x => 1);
$q.sety;
ok $p eqv $q, 'a punned role ignores a private-attribute difference';
nok S.new(x => 1) eqv S.new(x => 2), 'a punned role compares public attributes';
my $r = S.new(x => 1);
$r.m;
ok S.new(x => 1) eqv $r, 'a method call on a punned role does not change eqv';

class F { has $.v }
nok (F.new(v => 1) but S) eqv (F.new(v => 2) but S), 'a mixin compares the base instance';

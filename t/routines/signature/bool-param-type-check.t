use Test;

# A `Bool` argument against a parameter typed with one of its core types
# (`Bool`, `Int`, `Numeric`, `Real`, `Cool`, `Any`, `Mu`, with or without
# `:D`) is answered without the full type check. Every neighbouring shape
# must still give rakudo's answer: a type object, a smiley, a Failure, a
# Junction (which autothreads), Nil, the enum spelling, a coercion type, a
# subset, an allomorph, and the types `Bool` does NOT satisfy (`int`,
# `Enumeration`, `Str`).
plan 23;

sub ok-bind(&code) { try { code(); True } // False }

class R {
    method b(Bool:D $x) { "b:$x" }
    method i(Int:D $x)  { "i:$x" }
    method n(Numeric $x) { "n:$x" }
    method c(Cool:D $x) { "c:$x" }
    method u(Bool:U $x) { "u:" ~ $x.^name }
    method s(Str:D $x)  { "s:$x" }
}
my $r = R.new;
for ^2 -> $pass {
    is (True, False).map({ $r.b($_) }).join(','), 'b:True,b:False', "Bool:D takes True and False (pass $pass)";
}
is $r.i(True), 'i:True', 'Int:D takes a Bool';
is $r.n(False), 'n:False', 'Numeric takes a Bool';
is $r.c(True), 'c:True', 'Cool:D takes a Bool';
is $r.u(Bool), 'u:Bool', 'Bool:U takes the type object';
nok ok-bind({ $r.b(Bool) }), 'Bool:D refuses the type object';
nok ok-bind({ $r.u(True) }), 'Bool:U refuses True';
nok ok-bind({ $r.b(1) }), 'Bool:D refuses an Int';
nok ok-bind({ $r.b('True') }), 'Bool:D refuses a Str';
nok ok-bind({ $r.s(True) }), 'Str:D refuses a Bool';
nok ok-bind({ $r.b(Nil) }), 'Bool:D refuses Nil';
nok ok-bind({ $r.b(Failure.new('x')) }), 'Bool:D refuses a Failure';

my @seen;
sub jb(Bool:D $x) { @seen.push($x); $x }
my $j = jb(True | False);
is @seen.sort.join(','), 'False,True', 'a Junction autothreads over Bool:D';
ok $j ~~ Junction, '...and the result is a Junction';

is Bool::True ~~ Bool && Bool::True ~~ Int, True, 'the enum spelling is a Bool and an Int';

sub nat(int $x) { $x }
is nat(True), 1, 'an int parameter still binds a Bool by value';
nok True ~~ int, 'a Bool is not the native int type';
nok True ~~ Enumeration, 'a Bool is not an Enumeration';

sub co(Str(Bool) $x) { $x }
is co(True), 'True', 'a coercion type from Bool coerces';

subset Yes of Bool where *.so;
sub yes(Yes $x) { 'yes' }
is yes(True), 'yes', 'a Bool subset admits a matching value';
nok ok-bind({ yes(False) }), 'a Bool subset refuses a non-matching value';

sub is-str(Str:D $x) { $x.^name }
is is-str(<1>), 'IntStr', 'an allomorph still binds to Str:D';

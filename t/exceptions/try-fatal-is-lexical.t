use Test;

# A `try` block behaves like `use fatal` only LEXICALLY (#11391): a Failure
# stored or sunk inside a routine *called* from the try stays soft, while one
# stored directly in the try body throws. Every row was checked against
# `raku`.

plan 12;

sub stores-scalar { my $x = "Inf".Int; 1 }
try { stores-scalar() }
nok $!.defined, 'a Failure stored in a scalar by a called sub stays soft';

{
    my $r = try { stores-scalar() };
    is $r, 1, '... and the call returns its value';
}

sub stores-containers { my %h; %h<a> = "Inf".Int; my @a = (1, "x".Int); 1 }
try { stores-containers() }
nok $!.defined, 'hash-element and array stores in a called sub stay soft';

class C { method m { my $x = "Inf".Int; 1 } }
try { C.m }
nok $!.defined, 'a Failure stored by a called method stays soft';

sub maps-closure { (1, 2).map({ my $y = "x".Int; 1 }).eager }
try { maps-closure() }
nok $!.defined, 'a closure built by a called sub is not fatal either';

grammar G {
    token TOP { <number> }
    token number { 'Inf' | \d+ }
}
class Actions {
    method TOP($/) { make 'depth' => $<number>.made }
    method number($/) { make (~$/).Int }
}
{
    my $made = try { G.parse('Inf', :actions(Actions.new)).made };
    nok $!.defined, 'a grammar action storing a Failure under a try stays soft';
    isa-ok $made.value, Failure, '... and the pair holds the Failure';
}

# The try body itself is still fatal.
try { my $x = "Inf".Int; }
ok $! ~~ Exception, 'a Failure stored directly in the try body throws';

try { my @a = (1, "x".Int, 3); 1 }
ok $! ~~ Exception, 'a Failure in a list literal in the try body throws';

sub takes-two($a, $b) { 1 }
try { takes-two(1, "x".Int); }
ok $! ~~ Exception, 'a Failure passed as an argument in the try body throws';

try { (1, 2).map({ my $y = "x".Int; 1 }).eager }
ok $! ~~ Exception, 'a closure written in the try body is fatal';

# Once the try is left, nothing is fatal any more.
{
    my $x = "Inf".Int;
    ok $x ~~ Failure, 'a store after the try stays soft';
    $x.so;
}

use Test;

# Both compile-time private-method checks walk the whole method body
# (ADR-0137 typed visitor): an untrusted `$o!Owner::m` and a `self!m` naming
# no private method are rejected wherever rakudo rejects them, not only in
# the positions an older hand-rolled walker listed.

plan 7;

my class Owner { method !p { 1 } }

throws-like 'my class B1 { method m($o) { given $o!Owner::p { } } }',
    X::Method::Private::Permission, 'untrusted call as a given topic';
throws-like 'my class B2 { method m($o) { my %h = x => $o!Owner::p } }',
    X::Method::Private::Permission, 'untrusted call as a hash value';

throws-like 'my class C1 { method m { given self!nope { } } }',
    X::Method::NotFound, 'missing private method as a given topic';
throws-like 'my class C2 { method m { sub f { self!nope } } }',
    X::Method::NotFound, 'missing private method in a nested sub';
throws-like 'my class C3 { method m { my $c = -> $y = self!nope { } } }',
    X::Method::NotFound, 'missing private method in a parameter default';

# `self` inside an anonymous method still names the enclosing class's
# private methods.
my class AnonMethod {
    method !p { 42 }
    method m { my $am = method { self!p }; self.$am() }
}
is AnonMethod.new.m, 42, 'an anonymous method calls the class private method';

# A trusted caller passes in the newly searched positions too.
my class Trusted { ... }
my class Truster { trusts Trusted; method !secret { 'ok' } }
my class Trusted {
    method m($t) { my %h = x => $t!Truster::secret; %h<x> }
}
is Trusted.new.m(Truster.new), 'ok', 'a trusted call as a hash value';

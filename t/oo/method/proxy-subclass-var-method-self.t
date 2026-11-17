use Test;

# A Proxy subclass's `.VAR` object is an instance of its class; inside the
# subclass's methods `self` is the Proxy as a container (reading it FETCHes);
# `Proxy.new` builds the container without BUILDALL (#17459).

plan 14;

my class P is Proxy {
    has $.label = 'lbl';
    has Int $.n;
    has $.d is default(5);
    method who { self.^name }
    method var-name { self.VAR.^name }
    method me-isa { self ~~ P }
}
my class Q is P { }

my $fetched = 0;
my $p := P.new(FETCH => -> $ { $fetched++; 42 }, STORE => -> $, $ { });

ok $p.VAR ~~ P, '.VAR smartmatches its class';
ok $p.VAR ~~ Proxy, '... and Proxy';
ok $p.VAR.isa(P), '.isa of its class';
ok $p.VAR.isa(Proxy), '... and of Proxy';
is $fetched, 0, 'none of that FETCHes';

is $p.VAR.who, 'Int', 'self in a method FETCHes when read';
is $fetched, 1, '... once';
is $p.VAR.var-name, 'P', 'self.VAR is the Proxy';
nok $p.VAR.me-isa, 'self ~~ P reads the value';

is-deeply $p.VAR.label, Any, 'Proxy.new does not run an attribute default';
is-deeply $p.VAR.n, Int, 'a typed attribute holds its type object';
is $p.VAR.d, 5, 'an is default value applies';
my $q := Q.new(FETCH => -> $ { 1 }, STORE => -> $, $ { });
is $q.VAR.d, 5, 'an inherited is default value applies';

throws-like { P.new(FETCH => -> $ { 1 }, STORE => -> $, $ { }, label => 'x') }, X::AdHoc,
    message => "Unexpected named argument 'label' passed", 'Proxy.new refuses other named arguments';

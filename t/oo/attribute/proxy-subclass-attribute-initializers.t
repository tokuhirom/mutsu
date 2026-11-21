use Test;

# `Proxy.new` builds the proxy natively and never runs BUILDALL, so a Proxy
# subclass's attribute initializers do not run: an attribute reads as its
# type object, or as its `is default` value when it has one (#17925).

plan 8;

my class P is Proxy {
    has Bool $!flag = False;
    has $.label = 'dflt';
    has $.d is default(7);
    method flag { $!flag }
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE) }
}

my $p := P.new(FETCH => -> $ { 1 }, STORE => -> $, $ { });
is $p.VAR.flag.raku, 'Bool', 'a private attribute initializer does not run';
is $p.VAR.label.raku, 'Any', 'a public attribute initializer does not run';
is $p.VAR.d, 7, 'an is default attribute reads its default';
is $p, 1, 'the proxy still fetches';

my class Q is Proxy {
    has $.x = 3;
    has Int $.n = 42;
}

my $q := Q.new(FETCH => -> $ { 2 }, STORE => -> $, $ { });
ok $q.VAR.x === Any, 'the inherited Proxy.new skips the initializer';
ok $q.VAR.n === Int, 'a typed attribute reads as its type object';
is $q, 2, 'the inherited-constructor proxy fetches';

my $q2 := Q.new(FETCH => -> $ { 5 }, STORE => -> $, $ { });
ok $q2.VAR.x === Any, 'a second instance skips the initializer too';

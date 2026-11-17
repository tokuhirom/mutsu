use Test;

# A private method called on a `Proxy` subclass's `.VAR` object
# (`callwith(...).VAR!SET-SELF(%h)`, the AttrX::Mooish `AttrProxy.new` idiom)
# runs the class's private method with the Proxy as `self`.

plan 9;

my class AttrProxy is Proxy {
    has $.val;
    has $!tag;
    has $.instance;
    method !SET-SELF(%attrinit) is raw {
        $!tag := %attrinit<tag>;
        $!instance := %attrinit<instance>;
        self
    }
    method new(:$FETCH, :$STORE, *%attrinit) is raw {
        callwith(:$FETCH, :$STORE).VAR!SET-SELF(%attrinit)
    }
    method tag { $!tag }
    method !secret { 'secret ' ~ $!tag }
    method reveal { self.VAR!secret }
}

my $store = 0;
my $p := AttrProxy.new(
    FETCH => -> $ { 42 },
    STORE => -> $, $v { $store = $v },
    tag => 't1',
    instance => 'inst',
);
is $p, 42, 'the proxy FETCHes';
is $p.VAR.tag, 't1', 'the private initializer bound a private attribute';
is $p.VAR.instance, 'inst', 'and a public one';
is $p.VAR.reveal, 'secret t1', 'a private method called from a public one';
$p = 7;
is $store, 7, 'the proxy STOREs';

for ^3 -> $i {
    my $q := AttrProxy.new(FETCH => -> $ { $i }, STORE => -> $, $ { }, tag => "x$i");
    is $q.VAR.tag, "x$i", "fresh proxy $i keeps its own attributes";
}

throws-like '$p.VAR!AttrProxy::secret', X::Method::Private::Permission,
    'an outside caller is still refused';

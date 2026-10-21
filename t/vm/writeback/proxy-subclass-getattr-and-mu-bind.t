use Test;
use nqp;

plan 5;

# AttrX::Mooish's AttrProxy: a Proxy subclass whose attributes are read with
# nqp::getattr on the Proxy itself (no FETCH), bound to a `my Mu` variable
# (no FETCH either, Mu accepts anything) (#14266).
my @fetched;
my class P is Proxy {
    has Bool $!f is default(False);
    has $.h is default(3);
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE) }
}

my $p := P.new(FETCH => -> $ { @fetched.push: 1; 42 }, STORE => -> $, $v { });
is nqp::getattr($p, P, '$!f'), False, 'getattr reads a Proxy subclass attribute';
is nqp::getattr($p, P, '$!h'), 3, 'getattr reads a public attribute with its default';
is @fetched.elems, 0, 'getattr does not FETCH';

sub bind-mu() {
    my Mu $m;
    my $keep := ($m := Proxy.new(FETCH => -> $ { @fetched.push: 2; 42 }, STORE => -> $, $v { }));
    @fetched.elems
}
is bind-mu(), 0, 'binding a Proxy to `my Mu` does not FETCH';

nqp::bindattr($p, P, '$!f', True);
is nqp::getattr($p, P, '$!f'), True, 'bindattr writes the Proxy subclass attribute';

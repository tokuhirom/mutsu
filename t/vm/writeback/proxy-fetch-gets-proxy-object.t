use Test;
use nqp;

# FETCH receives the Proxy object itself: `nqp::decont($proxy)` inside it is
# that object, not another FETCH. A Proxy subclass whose `new` `callwith`s
# `Proxy.new` gets an instance of itself back, not a FETCHed value
# (AttrX::Mooish's `AttrProxy`, #14162).

plan 3;

my $plain := Proxy.new(
    FETCH => sub ($proxy) { my $self := nqp::decont($proxy); $self.VAR.^name },
    STORE => sub ($, $) { });
is $plain, 'Proxy', 'nqp::decont of the FETCH argument is the Proxy, not another FETCH';

class Tagged is Proxy {
    has $.tag = 'T';
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE) }
}
my $made := Tagged.new(FETCH => sub ($) { 42 }, STORE => sub ($, $) { });
is $made.VAR.^name, 'Tagged', 'callwith from a subclass new makes the subclass';
is $made, 42, 'and FETCHes when read';

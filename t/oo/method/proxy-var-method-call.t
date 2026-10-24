use Test;

# `.VAR` of a Proxy subclass instance is the container, so a method called on
# it runs on the Proxy and does not FETCH (#14855). Repeated calls take the
# cached dispatch and must agree with the first.

plan 9;

my class P is Proxy {
    has $!set is default(False);
    method is-set { ⚛$!set }
    method mark { $!set = True }
}
my $p := P.new(FETCH => -> $ { 42 }, STORE => -> $, $ { });

nok $p.VAR.is-set, 'a fresh proxy is not set';
$p.VAR.mark;
ok $p.VAR.is-set, 'the method ran on the container and changed its state';
is $p.VAR.^name, 'P', '.VAR is the proxy container, not the FETCHed value';
is $p, 42, 'reading the proxy itself still FETCHes';
is $p.VAR.WHAT.^name, 'P', '.VAR.WHAT is the Proxy subclass';

my @seen;
@seen.push($p.VAR.is-set) for ^3;
is-deeply @seen, [True, True, True], 'repeated calls agree';

# A method with the name of a built-in on a user class keeps overriding it.
class Q { method elems { 'mine' } }
is Q.new.elems, 'mine', 'a user method named like a built-in answers';
is Q.new.elems, 'mine', 'and again from the cache';
class R is Q { }
is R.new.elems, 'mine', 'and for a subclass';

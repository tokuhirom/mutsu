use Test;

plan 4;

# `my \s = (Pkg.WHO<k> := Proxy.new(...))` keeps the Proxy unfetched, as
# rakudo does: binding into a package stash answers the bound Proxy itself.

my $fetches = 0;
sub lazy() {
    Proxy.new(FETCH => -> $ { $fetches++; 42 }, STORE => -> $, $v { die "read-only" })
}

class A { }
my \s = (A.WHO<P> := lazy());
is $fetches, 1, 'only the sub return fetched it';

$fetches = 0;
class B { }
my \t = (B.WHO<P> := Proxy.new(FETCH => -> $ { $fetches++; 7 }, STORE => -> $, $v { die "read-only" }));
is $fetches, 0, 'binding into the stash does not FETCH';
is B.WHO<P>, 7, 'a read FETCHes';
dies-ok { B.WHO<P> = 1 }, 'an assignment goes through STORE';

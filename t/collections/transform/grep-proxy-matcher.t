use Test;

# grep's matcher binds to a `Mu $test` parameter, which reads a Proxy once:
# `@list.grep($obj.state)` where `state` is an `is rw` method returning a Proxy
# greps by the value FETCH answers. Tinky's transition role does
# `$transitions.grep(self.state)`.

plan 3;

class S { has $.name }
class O {
    has $.st;
    method state() is rw {
        my $s = self;
        Proxy.new(FETCH => method () { $s.st }, STORE => method ($v) { })
    }
}
my $a = S.new(name => 'a');
my @items = $a, S.new(name => 'b'), $a;
my $o = O.new(st => $a);

is @items.grep($o.state).elems, 2, 'a Proxy matcher greps by its fetched value';
my $fetches = 0;
my $p := Proxy.new(FETCH => method () { $fetches++; 'x' }, STORE => method ($v) { });
is <x y x>.grep($p).elems, 2, 'a bound Proxy matcher works';
is $fetches, 1, 'and is fetched once, not per element';

use Test;

plan 6;

# Assigning to a stash entry that is *bound* (not a container): a bound
# Proxy is stored through its STORE, a bound type object is immutable.

class A { }
class B { }

A.WHO<Sub> := B;
dies-ok { A.WHO<Sub> = Any }, 'a bound type object is immutable';
is A.WHO<Sub>.^name, 'B', 'and keeps its value';

my $stored;
A.WHO<P> := Proxy.new(FETCH => -> $ { 42 }, STORE => -> $, $v { $stored = $v });
A.WHO<P> = 7;
is $stored, 7, 'assignment goes through the bound Proxy STORE';
is A.WHO<P>, 42, 'the Proxy stays bound';

A.WHO<RO> := Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { die "read-only" });
dies-ok { A.WHO<RO> = 2 }, 'a refusing STORE dies';

A.WHO<Re> := B;
A.WHO<Re> := A;
is A.WHO<Re>.^name, 'A', 'a bind still rebinds the entry';

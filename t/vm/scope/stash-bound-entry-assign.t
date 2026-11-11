use Test;

plan 5;

# Assigning to a stash entry that holds a bound Proxy goes through its
# STORE; assigning a plain entry replaces it.

class A { }
class B { }

A.WHO<Plain> = B;
A.WHO<Plain> = A;
is A.WHO<Plain>.^name, 'A', 'an assigned entry can be reassigned';

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

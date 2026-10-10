use Test;

# Signature's arity, count, params and returns are rows of the method table
# (ADR-11276 §9.55).

plan 8;

sub f(Int $a, $b?, :$c) { }
my $s = &f.signature;
is $s.arity, 1, 'arity counts required positionals';
is $s.count, 2, 'count counts all positionals';
is $s.params.elems, 3, 'params lists every parameter';
is $s.returns.raku, 'Mu', 'returns defaults to Mu';
is :(Int $a, *@r).count, Inf, 'a slurpy makes count Inf';
is :(Int $a, *@r).arity, 1, 'a slurpy is not required';
sub g(--> Str) { 'x' }
is &g.signature.returns.raku, 'Str', 'declared return type';
is :().params.elems, 0, 'empty signature has no params';

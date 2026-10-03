use Test;

# #11412: an enum declared inside a sub keeps its bare keys lexical to the
# sub; the caller's same-named key must survive the call.
plan 3;

enum Volume (:Silent(-2) :Normal(0));
sub f { enum M2 <Normal X>; 1 }
f();
is Normal.raku, 'Volume::Normal', 'caller key survives a sub declaring a same-named enum key';

sub g($x) { enum M3 <Silent Y>; $x }
g(1);
is Silent.raku, 'Volume::Silent', 'same with a positional parameter';

sub h { enum M4 <Normal Z>; Normal.raku }
is h(), 'M4::Normal', 'inside the sub the sub-local key wins';

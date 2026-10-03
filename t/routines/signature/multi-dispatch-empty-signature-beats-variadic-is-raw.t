use Test;

plan 5;

# `*@a is raw` keeps the elements' containers but needs no writable argument,
# so it does not make its candidate narrower than `()`: a call with no
# arguments picks `()` (P5chomp's `chomp()` that dies).
multi sub f()            { 'empty' }
multi sub f(*@a is raw)  { "slurped {+@a}" }
is f(), 'empty', 'no arguments: the empty signature wins';
is f(1, 2), 'slurped 2', 'arguments still reach the variadic candidate';

proto sub g(|) {*}
multi sub g(*@a is raw) { 'variadic' }
multi sub g()           { die 'needs arguments' }
dies-ok { g() }, 'declaration order does not matter';

# An `is rw` scalar is still narrower than a plain one.
multi sub h($x is rw) { 'rw' }
multi sub h($x)       { 'plain' }
my $v;
is h($v), 'rw', 'is rw scalar wins for a variable';
is h(42), 'plain', 'and loses for a literal';

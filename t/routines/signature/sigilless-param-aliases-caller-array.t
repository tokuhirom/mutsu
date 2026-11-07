use Test;

plan 9;

# A sigilless parameter bound to an `@`-parameter of the caller aliases the
# caller's array: an element store through it is visible to the original
# variable two calls up (#16988). rakudo rejects the typed spellings at compile
# time, so only untyped and Positional-free signatures are used here.
sub store(\v) { v[0] = 99 }
sub forward(@x) { store(@x) }
my @a = 3, 1, 2;
forward(@a);
is-deeply @a, [99, 1, 2], 'untyped sigilless, one hop through an @ parameter';

sub store-code(&c, \v) { v[0] = 99 }
sub forward-code(&c, @x) { store-code(&c, @x) }
my @b = 3, 1, 2;
forward-code({ 1 }, @b);
is-deeply @b, [99, 1, 2], 'a code parameter first';

sub store-named(\v, :$n) { v[0] = 99 }
sub forward-named(@x) { store-named(@x, :n) }
my @c = 3, 1, 2;
forward-named(@c);
is-deeply @c, [99, 1, 2], 'a named argument after it';

sub store-many(\v) { v[0] = 10; v[1] = 20; v.push(30) }
sub forward-many(@x) { store-many(@x) }
my @d = 1, 2, 3;
forward-many(@d);
is-deeply @d, [10, 20, 3, 30], 'several stores and a push';

sub store-hash(\h) { h<k> = 5 }
sub forward-hash(%x) { store-hash(%x) }
my %h = a => 1;
forward-hash(%h);
is-deeply %h, {a => 1, k => 5}, 'a hash, through a % parameter';

# Three hops.
sub hop3(@z) { store(@z) }
sub hop2(@y) { hop3(@y) }
sub hop1(@x) { hop2(@x) }
my @e = 3, 1, 2;
hop1(@e);
is-deeply @e, [99, 1, 2], 'three hops';

# Unchanged behaviour: a top-level variable, and the sigiled parameter.
my @f = 3, 1, 2;
store(@f);
is-deeply @f, [99, 1, 2], 'a top-level array (one hop)';
sub store-sigiled(@v) { @v[0] = 99 }
sub forward-sigiled(@x) { store-sigiled(@x) }
my @g = 3, 1, 2;
forward-sigiled(@g);
is-deeply @g, [99, 1, 2], 'an @ parameter two hops';
my @h = 3, 1, 2;
sub no-alias(\v) { my @copy = v; @copy[0] = 7; v[0] }
is no-alias(@h), 3, 'a copy of the argument is still independent';

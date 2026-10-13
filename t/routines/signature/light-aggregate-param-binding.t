use Test;

# #12526: a plain `@x`/`%x` parameter binds on the positional-light path
# without mirroring itself, its `.VAR` source or its thread record into the
# callee env unless something reads them. Each case below reads one of them.

plan 14;

# A method call on the parameter, and a named element store, resolve it by
# name: the body must see (and mutate) the caller's container.
sub push-it(@x) { @x.push(4); @x.elems }
my @a = 1, 2, 3;
is push-it(@a), 4, 'a method call on a light-bound @x sees the container';
is @a.join(','), '1,2,3,4', 'and mutates the caller array';

sub store-it(%h) { %h<z> = 9; %h.elems }
my %h = a => 1;
is store-it(%h), 2, 'a named element store on %h sees the container';
is %h<z>, 9, 'and reaches the caller hash';

sub bump(@x) { @x[0]++; @x[1] = 'b'; @x }
my @b = 1, 2;
bump(@b);
is @b.join(','), '2,b', 'element increment and store reach the caller';

sub whole(@x) { @x = 7, 8; @x }
my @c = 1;
whole(@c);
is @c.join(','), '7,8', 'a whole-array assignment stores in place';

# `.VAR.name` is the source record's reader.
sub var-name(@x) { @x.VAR.name }
is var-name(@a), '@a', '.VAR.name of a variable argument is its name';
is var-name([1]), 'element', '.VAR.name of a literal argument is element';
sub var-name-closure(@x) { my &c = { @x.VAR.name }; c() }
is var-name-closure(@a), '@a', '.VAR.name from a nested closure';

# The topic of a caller's `for` is readonly; a callee that names `$_` gets a
# fresh writable one, and one that never names it leaves the mark intact.
sub own-topic() { $_ = 9; $_ }
sub no-topic(@x) { @x.elems }
my @seen;
for <p q> {
    @seen.push: own-topic();
    @seen.push: no-topic(@a);
    @seen.push: (try { $_ = 'w'; 'writable' }) // 'readonly';
}
is @seen.join(','), '9,4,readonly,9,4,readonly', 'the caller topic stays readonly around light calls';

# A thread spawned from a block the routine made sees this call's binding.
sub spawn(@P) { start { @P[0] } }
my @r = await (1, 2, 3).map: -> $i { spawn([$i * 10]) };
is @r.join(','), '10,20,30', 'a start block sees each call of an @ parameter';

# A typed container keeps its element constraint through the bind, and an
# untyped argument does not inherit an outer typed variable's.
my Int @typed = 1, 2;
sub assign-str(@typed) { @typed = 'x'; @typed }
throws-like { assign-str(@typed) }, X::TypeCheck::Assignment,
    'a typed container argument keeps its element type';
my @plain = 1, 2;
is assign-str(@plain).join, 'x', 'an untyped argument takes no outer constraint';

# A `|` argument still slips.
sub count(*@a) { @a.elems }
my $l = (1, 2);
is count(|$l, $l), 3, 'a slip argument still flattens';

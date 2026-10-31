use Test;

# A sigilless parameter is an alias of the caller's variable, so the variable's
# declared type applies through it. The type record follows the variable, not
# the frames that happen to call the routine (ADR-13757 step 4).

plan 5;

sub store-via-alias(\x) { x[0] = 's' }
sub typed-caller() { my Int @q; store-via-alias(@q) }
throws-like { typed-caller() }, X::TypeCheck::Assignment,
    'a store through a sigilless alias checks the bound array\'s element type';

sub untyped-caller() { my @q; store-via-alias(@q); @q[0] }
is untyped-caller(), 's', 'an untyped array stays untyped through the alias';

# A same-named typed variable of an unrelated caller is not the alias.
sub plain(\x) { x[0] = 's'; x[0] }
sub outer-typed() { my Int @x; plain(my @y) }
is outer-typed(), 's', 'a caller\'s typed @x says nothing about the alias of @y';

# Hash value type through an alias.
sub store-hash(\h) { h<k> = 's' }
sub typed-hash-caller() { my Int %t; store-hash(%t) }
throws-like { typed-hash-caller() }, X::TypeCheck::Assignment,
    'a store through a sigilless alias checks the bound hash\'s value type';

# A closure assigns to the enclosing typed lexical, not to a typed variable of
# the frame that called it.
sub call-it(&c) { my Str $typed = 'x'; c() }
my Int $total = 0;
call-it({ $total = 5 });
is $total, 5, 'a closure stores through the lexical it closes over';

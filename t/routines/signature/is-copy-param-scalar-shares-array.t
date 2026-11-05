use Test;

# #15649: a `$x is copy` parameter gets a fresh Scalar around the SAME Array or
# Hash, so `.push` through it reaches the caller's aggregate; `@x is copy` and
# `%x is copy` copy the aggregate. The default-parameter forms go through the
# general binder, the others through the light call path.

plan 10;

sub push-gen($x is copy, $y = $x.elems) { $x.push(9); $x.elems }
my @arr = 1, 2;
is push-gen(@arr), 3, 'general binder: the copy sees the push';
is @arr.elems, 3, 'general binder: the caller sees it too';

sub push-light($x is copy) { $x.push(9); $x.elems }
my @b = 1, 2;
is push-light(@b), 3, 'light call: the copy sees the push';
is @b.elems, 3, 'light call: the caller sees it too';

sub reassign($x is copy, $y = 0) { $x = [7]; $x.elems }
my @c = 1, 2;
is reassign(@c), 1, 'assigning the copy does not touch the caller';
is @c.elems, 2, 'the caller is unchanged';

sub ac(@x is copy, $y = @x.elems) { @x.push(9); @x.elems }
my @d = 1, 2;
is ac(@d), 3, '@x is copy: the copy grows';
is @d.elems, 2, '@x is copy: the caller does not';

sub hset($h is copy, $y = 0) { $h<z> = 1; $h.elems }
my %h = a => 1;
is hset(%h), 2, 'a scalar copy of a Hash writes through';
is %h.elems, 2, 'and the caller sees the key';

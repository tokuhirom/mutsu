use Test;

# #15951: `$t.WHO<k> := v` / `$t.WHO<k> = v` on a `$` variable holding a type
# object writes into the type's stash, as the bareword and sigilless forms do.

plan 5;

class Foo {}
my $t = Foo;
$t.WHO<B> := 2;
is Foo.WHO<B>, 2, 'bind through a scalar variable';
$t.WHO<C> = 3;
is Foo.WHO<C>, 3, 'assign through a scalar variable';
Foo.WHO<D> = 4;
is Foo.WHO<D>, 4, 'assign through the bareword type';
sub f(Mu \type) { type.WHO<E> := 5 }
f(Foo);
is Foo.WHO<E>, 5, 'bind through a sigilless parameter';
is $t.WHO<C>, 3, 'read back through the scalar';

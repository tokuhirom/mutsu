use Test;

# #15965: a store into a package stash through `.WHO<k>` (assign or bind) makes
# the stash entry `k`, and no extra `$k` scalar key.

plan 8;

class Foo { our $x = 1; }
Foo.WHO<D> = 4;
Foo.WHO<E> := 5;
Foo.WHO<lower> = 6;

is Foo.WHO<D>, 4, 'assigned entry reads back';
is Foo.WHO<E>, 5, 'bound entry reads back';
is Foo.WHO<lower>, 6, 'a lowercase entry reads back';
is Foo.WHO.keys.sort.join(','), '$x,D,E,lower', 'no $-prefixed duplicates of the stored keys';
ok Foo.WHO<$x>:exists, 'the declared our $x is still a scalar key';
ok !(Foo.WHO<$D>:exists), 'there is no $D';

class Bar {}
Bar.WHO<k> = 1;
is Bar.WHO.keys.join(','), 'k', 'a package with one stored entry lists just it';
is Bar.WHO.elems, 1, 'and counts one';

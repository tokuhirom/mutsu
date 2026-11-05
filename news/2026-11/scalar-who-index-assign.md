# `$t.WHO<k> = v` writes into the stash

An element store through `.WHO` on a scalar variable holding a type object
(`my $t = Foo; $t.WHO<k> = v`, and the bind form) wrote into a throwaway copy
because it was compiled as an rw-accessor store on `$t`. `.WHO` is not an
accessor of the variable, so the store now takes the same path as `Foo.WHO<k> = v`
(#15951).

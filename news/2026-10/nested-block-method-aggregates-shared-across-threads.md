# A nested-block method's arrays and hashes are shared across threads

A method declared in a nested block of a class body (`class C { { my @a; method add($x) {
@a.push($x) } } }`) closes over that block's `@a`. #13044 put the block's scalars into
shared cells so that every thread sees one variable. Arrays and hashes were left out on the
assumption that they are their own containers. They are copy-on-write nodes, though, so a
push on a worker thread wrote its copy into that thread's snapshot of the method table. The
main thread never saw it once any role had been composed in the program, even an unrelated
one. `@` and `%` lexicals of such a block are now boxed in shared cells too, so
`Arr.add(1); await start { Arr.add(2) }; Arr.all` gives `1,2` as in Rakudo (#13056).

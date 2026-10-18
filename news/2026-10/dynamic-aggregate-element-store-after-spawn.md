# Dynamic hash/array element stores survive across threads

Once a program had started any thread, an element store into a dynamic
aggregate (`%*h<k> = v`, `@*a[i] = v`) made from a closure or a `.then`
callback could vanish: the store went to the cross-thread store's base-key
entry, while the frame owning the variable reads its `__mutsu_atomic_*` lane.
`my %*H; await start { 1 }; { %*H<c> = 1 }(); say %*H` printed `{}`.

This was the root cause of roast `S17-promise/then.t` intermittently reporting
`%*FOO` as `{}` (#13772). Dynamic aggregates now take the same atomic lane as
plain lexicals, so the store is seen by every reader.

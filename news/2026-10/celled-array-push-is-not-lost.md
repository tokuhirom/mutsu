# A push to an array a thread captured is no longer lost

`my @a = ^3; await start { @a[0] = 5; @a.push(1) }; say @a` printed `[5 1 2]`,
where raku prints `[5 1 2 1]`. Four threads racing `@a.push` against a routine
that took `@a` as a parameter kept only a fraction of the pushes: 154 to 665
of 1010 (#13078).

A `my @a` captured by a `start` lives in a shared cell. A push through the cell
went through `Gc::make_mut`, which copies the array whenever anything else
still references it: the worker's own slot, a parameter bound to it, an
element store's copy. The push then landed on the copy and the program went
on reading the original.

The push (and `unshift`, `append`, `prepend`, `pop`, `shift`, `splice`) now
writes into the array in place, under the cell's and the node's ADR-0068
locks, as rakudo's arrays grow in place. That removes the copy that used to
shield a reader that took no lock, so the maintainer approved it as a
memory-safety trade-off. Since ADR-0068 §20, readers of a shared array hold its
lock or read a snapshot. All six shapes now answer as raku does, and
`t/concurrency/concurrent-celled-push-is-not-lost.t` pins them.

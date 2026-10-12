# Same-named arrays in different routines stay separate on worker threads

#11306 reported that two routines' unrelated `my @o`, each pushed to from a
`start` block, read each other's contents. The cause was that worker-thread
pushes went into a cross-thread lane keyed only by the variable's name. That
was fixed along the way by #11343, which routes a push to a captured `@`
lexical through the lexical's own cell. The regression test for #11238 had
used a different name to avoid this bug. It now uses the same name, and it
also covers sibling routines' same-named arrays and two closure clones of one
routine's `%h`.

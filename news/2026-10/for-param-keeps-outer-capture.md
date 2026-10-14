# After `for … -> $b { }`, a routine reads the outer `$b` again

`my $b = 99; sub g { for 1 -> $b { }; $b }; say g()` printed `(Any)`, and so
did the same loop in a pointy block. A single loop parameter has no slot: the
loop binds it by name and restores the prior binding afterwards. But nothing
had captured the enclosing `$b` for the routine, so there was no prior binding
to restore. The routine never captured it because the compiler treated every
by-name read of a name that any loop of the chunk binds as the loop's own.

The capture is now forced for a read made while no loop binds the name
(#13391's mechanism), and the free-variable analysis keeps that capture. The
loop body's reads stay by-name and are never turned into upvalue reads. A
mainline named sub reads its captured lexicals ahead of the env, so its
lookup now steps aside while one of its running loops binds the name (#13420).

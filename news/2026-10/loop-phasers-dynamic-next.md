# Loop NEXT/UNDO/LEAVE phasers run for a `next` raised anywhere

A loop iteration's NEXT, UNDO and LEAVE phasers used to be wired to each
`next`/`last` statement written in the loop body at compile time. A `next`
raised by a closure or sub the body called, or one written inside a `try`,
ended the iteration without running them:

```raku
for 1..3 { NEXT { print "n$_ " }; my &c = { next if $_ == 2 }; c(); print "b$_ " }
# was: b1 n1 b3 n3      now (as rakudo): b1 n1 n2 b3 n3
```

The compiler now brackets the iteration's body with a new
`OpCode::LoopExitGuard`. When a `next`/`last`/`redo`/`return` signal unwinds
out of it, the VM runs the NEXT queue (only for a `next` aimed at this loop)
and then UNDO and LEAVE, and re-raises the signal, the way rakudo's loop
handlers do. The static per-`next` rewrite is gone. As a side effect, a
`next LABEL` aimed at an outer loop now runs the inner loop's LEAVE, and
`redo` and `return` run LEAVE/UNDO, matching rakudo (#10566).

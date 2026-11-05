# Pure Code methods dispatch through their registered rows

The pure method entry for code objects now resolves the receiver's class/MRO
and invokes its registered method-table row. The separate `Capture`/`clone`
name checks and direct handler selection are gone. Code, Block, Routine,
Method and Regex keep one source of truth for their methods, including the
pure cloning path used by `Mu.clone`.

The pure entry declines a nearest method that needs the interpreter rather
than bypassing it to run an ancestor's pure implementation. Closure cloning
continues to fork state, and Capture conversion keeps its Code-specific error.

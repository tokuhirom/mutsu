# A routine's return no longer copies its variables back to the caller

Until now, returning from a routine walked the callee's variables and copied
the ones it had changed back over the caller's. That copy-back was how a
callee's writes once reached its caller, and it was also how an unrelated
same-named variable of the callee could overwrite the caller's. ADR-12529
gave every such write its own route first: an outer lexical is written
through its shared cell, a dynamic through its declaration's cell, and a
write whose name is known only at run time (or a rebind of a caller's
`$*x`) is carried to the frame that declares it. With nothing left
depending on it, the copy-back is deleted from both the full and the light
call paths (§3, slice 41).

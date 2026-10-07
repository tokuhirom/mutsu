# Named subs share captured scalars passed to rw calls

A named sub that forwards a captured scalar to a call now causes the declaring
frame to box that scalar into a shared cell. Worker threads therefore update
the same binding through `is rw` parameters, including when an earlier lexical
scope used an atomic variable with the same name.

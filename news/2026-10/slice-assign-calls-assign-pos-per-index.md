# Slice assignment to a user Positional calls `ASSIGN-POS` per index

`@o[1, 2] = a, b` on an object whose class declares `ASSIGN-POS` passed the
whole index list to one `ASSIGN-POS` call (`expected Int but got List`). It now
makes one call per index, each taking the next value, and answers the list of
results. The `annotations` distribution's `Keeper` assigns through a slice of
symbols this way.

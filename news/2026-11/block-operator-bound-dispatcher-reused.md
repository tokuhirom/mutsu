# A block's own operator runs one bound code object per candidate

A call of a block's own `multi infix:<op>` runs its winning candidate with the
family's dispatcher bound as `&infix:<op>`, so a `samewith` or a call of the
operator inside the candidate reaches the family. That binding copied the
candidate's whole code object on every operator call, so the copy's own
caches (its parameter names, interned symbols) were rebuilt each time. The
bound copy is now kept per (candidate, dispatcher) pair and reused (ADR-18580
D3). `bench-multi-dispatch`'s operator section goes from 1.92G to 1.66G
instructions.

# any, all, one and none are rows of Any

The junction-constructing methods `.any`, `.all`, `.one` and `.none` were answered only by a name-matching arm in the
interpreter's method cascade. They are now pure rows of `Any`'s method table (ADR-11276 §9.134), reached by the
native fast path for a plain list and by the MRO walk for an instance or a type object. A list held in a scalar is
still the list: `my $l = (1, 2); $l.any` is `any(1, 2)`.

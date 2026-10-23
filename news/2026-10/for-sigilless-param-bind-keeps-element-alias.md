# `$x := p` of a sigilless `for` parameter keeps the element alias

A scalar bound to `-> \p` of a `for @a` loop received the element's value, so a later write to the element was not visible through it. The compiler now hands the parameter's raw cell to the bind (the loop keeps it in env under `\p`), as it does for `is rw` and `<->` parameters (#14789).

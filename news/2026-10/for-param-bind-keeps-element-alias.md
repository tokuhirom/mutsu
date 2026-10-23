# `$x := $p` of an aliasing `for` parameter keeps the element alias

A scalar bound to a `for @a` parameter that aliases the element (`is rw`, `<->`) received the element's value, so a later write to the element was not visible through it. A scalar bind now takes the raw cell of such a parameter, as it already did for the implicit topic (#14766). A sigilless parameter is #14789.

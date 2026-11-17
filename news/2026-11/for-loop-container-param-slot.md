# `for` parameters with `@` or `%` are local slots, and assignment keeps the container's type

A pointy `for` loop's `$` and sigilless parameters are local slots of the loop
body (#17530, #17882). An `@`/`%` parameter (`for @rows -> @r`) now is one
too, so every single-parameter loop reads its parameter from a slot instead
of the by-name binding.

Giving them a slot exposed a gap in the slot store: a whole-container
assignment through an `@`/`%` variable (`%a = %other`) wrote the new contents
into the existing container but also gave it the type of the shaped new
contents, so an object hash `%h{Any}` bound to a parameter became a plain
`Hash`. The store now keeps the container's own key/value type when the new
contents carry none, as an assignment does in rakudo (ADR-12529).

# `set` / `bag` / `mix` flatten nested lists like a `*@` slurpy

`set((1, (2, 3)))` kept `(2, 3)` as one element, and `set(1..3)` kept the Range;
rakudo flattens the non-itemized lists of a `*@` slurpy recursively (an Array's
own elements, being itemized, stay whole) and a finite Range is its values. The
three functions now flatten their arguments with the slurpy flattening the
signature binder uses. Red's `set $ast.find-column-name`, a nested
`(("a",), ("b",).Seq)`, was a Set of two lists, so `$lcols ∩ $rcols` was always
empty and the AND optimizer's rules never fired (`t/06-optimizer`).

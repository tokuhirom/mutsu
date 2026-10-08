# BIND-KEY and BIND-POS are method-table rows

Binding a hash or array element to a variable (`%h<k> := $x`, `@a[0] := $x`) is now answered by `Hash.BIND-KEY` and
`Array.BIND-POS` rows instead of two ~150-line arms of the VM's `CallMethodMut`. The row gets the call's argument sources
through the receiver place and always writes the container's shared node, so every alias of the hash or array sees the bound
element. `Set`, `Bag` and `Mix` refuse `BIND-KEY` with `X::Bind` from rows too (ADR-11276 §9.41, part of #12387).

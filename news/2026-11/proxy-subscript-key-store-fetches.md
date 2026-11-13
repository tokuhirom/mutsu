# A Proxy subscript in an element store, and a nested Proxy pair key, are read by value

An element store or increment through a `Proxy` subscript (`%h{$p} = v`,
`%h{$p}++`, `@a[$q] = 9`) used the `Proxy` itself as the key -- `{Proxy => 1}` --
because only the read side FETCHed it; it now FETCHes the subscript first, as
rakudo does. A pair key whose FETCH answers another container (a `Proxy`
subclass holding a `Proxy`, the annotations module's `Binder`) was FETCHed only
one level and keyed by the inner `Proxy`; it is now followed down to its value.

Fixes annotations `t/01-containers.rakutest` ("Keeper": `%@keeper` builds
`Map.new: reverse @of Z=> self.Seq` from such slots).

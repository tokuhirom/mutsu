# `my \s = (Pkg.WHO<k> := Proxy.new(...))` keeps the Proxy unfetched

A sigilless declaration whose initializer binds into a package stash
(`my \symbol = (type.WHO<ResultSeq> := Proxy.new(...))`) now binds the
Proxy itself, as rakudo does, instead of FETCHing it at the declaration. Red
relies on this to publish a lazily built query class: the early FETCH built
the class eagerly and replaced the read-only Proxy, so assigning to
`$model.WHO<ResultSeq>` no longer reached its STORE.

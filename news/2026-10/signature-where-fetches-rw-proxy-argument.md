# A slurpy `where` clause sees an `is rw` call's value, not its Proxy

`method specialise($self, +@ret where .all ~~ Red::AST)` called as
`Person.^specialise(Person.id)` died with "Cannot resolve caller": the argument
is the Proxy an `is rw` accessor returns, and the slurpy's `where` clause (in
multi-candidate matching, and again in a plain method bind) tested the Proxy
instead of the value it fetches. A Proxy that binds a sigiled slurpy is now
fetched, as raku's decontainerizing bind does (#14094).

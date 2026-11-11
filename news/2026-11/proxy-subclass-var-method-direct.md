# A Proxy subclass's method on `.VAR` runs without the method cascade

`$proxy.VAR.is-set` on a `Proxy` subclass (AttrX::Mooish's `AttrProxy` asks
`.VAR.is-set`, `.VAR.build-acquire`, `.VAR.store-value` ... on every lazy
attribute access) answered `.VAR` on the pseudo-method lane, but the method
call that followed had no receiver shape the VM's lanes knew, so it ran the
interpreter's whole method cascade before reaching the subclass's compiled
method. The VM now dispatches it directly when no built-in method of that name
exists for the subclass, the name is not one of its attribute accessors, no
method is wrapped and no user `^find_method` intercepts dispatch.

The issue's repro drops from ~33k to ~29k instructions per iteration and
`MUTSU_VM_STATS` no longer lists these methods as interpreter fallbacks; what
remains is the compiled method call itself (#15737). Divergences found on the
way (a Proxy `.VAR` object failing `~~ P`, `self` not FETCHing inside its
methods, attribute defaults on `Proxy.new`) are #17459.

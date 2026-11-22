# `::($a)::($b)` is one symbolic name

`::($a)::($b)` used to be desugared to `::($a).WHO{$b}`. The stash was built
inside the method-call machinery, where the frames that `CALLER` and `OUTER`
need are gone, so `f({ ::('CALLER')::('$*bar') })` and
`::('OUTER')::('$*x')` came back empty. The construct is now what rakudo
makes of it, a single `Term::Name(EmptyEdge, Expression($a), Expression($b))`,
looked up at the site that wrote it. A name whose leading parts are
`CALLER`/`CALLERS`/`OUTER`/`OUTERS`/`UNIT` and whose last part is sigiled
walks the caller frames or the baked scope chain, as `CALLERS::('$_')`
already did. `CALLER::('$*bar')`, which used to die with "No such symbol",
works the same way.

A routine bound into the setting's stash (`&CORE::none := &f`,
`CORE::<&none> := &f`) is now what `&none` and `&CORE::none` return wherever
no declaration shadows it. A bare `OUTER::` is the pad one scope out
(`OUTER::MY::`), so `OUTER::.values` holds the outer scope's values.

roast `S02-names/pseudo-6d.t` and `6.c/S02-names/pseudo-6c.t` are down to 7
failures each (#18742). Pin: `t/vm/scope/pseudo-indirect-parts.t`.

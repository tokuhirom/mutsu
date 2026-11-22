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

Inside an `EVAL`, `&SETTING::not` is the routine of the scope the EVAL runs
in, so a lexical `sub not` is found, as rakudo compiles the snippet against
that scope. `&CALLER::SETTING::not` called from a frame an EVAL compiled
works the same way. `&CALLER::($p)::f` used to drop its `CALLER` prefix; it
now keeps it.

roast `S02-names/pseudo-6d.t` and `6.c/S02-names/pseudo-6c.t` are down to 3
failures each (#18742). The three left (`CALLER::OUTER::` and `CALLER::UNIT::`)
need the caller frame's static link (#18787). Pins:
`t/vm/scope/pseudo-indirect-parts.t`, `t/vm/scope/pseudo-setting-in-eval.t`.

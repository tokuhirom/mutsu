# A role mixed into `$*ARGFILES` works: `callsame`, `lines()`/`get()` and `-M`

LN mixes `IO::CatHandle::AutoLines` into `$*ARGFILES` from `sub EXPORT`. Three
gaps kept the line counter at 0: `callsame` in a role method mixed into an
`IO::Handle` re-entered the mixin forever (the native `IO::Handle` method is now
the base behind it); the `lines()` / `get()` subs ignored the mixin's override
(they are `$*ARGFILES.lines` / `.get`); and a bare `-MModule` loaded the module
outside the program's unit, so what its `EXPORT` did to a dynamic variable was
lost (`-MModule` is now the `use Module;` it stands for, like `-M'Module args'`
already was). All of LN's baseline files now match raku (`t/01-no-reset.t` has no
rakudo baseline: raku cannot find the dependency without a site install either).

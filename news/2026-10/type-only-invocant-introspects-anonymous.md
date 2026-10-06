# A type-only invocant introspects as anonymous

`method m(K:D: Int $c --> Int)` showed its invocant as `K:D $self` in `.signature.raku`
(`:(K:D $self:: Int $c, ...)`), as `K:D $self` in `Parameter.raku`, and as `$self` in
`.signature.params.map(*.name)`. The source never names that invocant, and rakudo shows an
anonymous one: `:(K:D $:: Int $c, *%_ --> Int)`, `K:D $:`, and an empty name.

mutsu stores a type-only invocant that has to survive (a `:D` / `:U` smiley, a `::T` capture) as a
parameter named `self`, tagged with the parser's `implicit-invocant` trait, because `self` is the
env key the binder fills. `param_def_to_sig_param` now reads that tag
(`ParamDef::is_implicit_invocant`) and gives the introspection parameter an empty name, as it
already did for the smiley-less `K:` form, which the parser drops. A user-written `$self:` or
`Str:D $a:` keeps its name, and binding and dispatch on the invocant are unchanged.

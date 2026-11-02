# `Parameter.nominal_type`, `.of`, `.coerce_type`, `.untyped`, `.onearg`, `.is-item`

Six `Parameter` methods that Rakudo declares were missing, so calling them died. They are attributes
set from the parsed parameter (`nominal_type` drops a coercion's source type, `coerce_type` is the
coercion's target or `Mu`, `of` is the type itself, `untyped` is a `Mu` parameter, `onearg` is `+@a`)
and rows of `Parameter`, so `.can` reports them too. `is-item` is always `False` for now: `is item`
is not yet an accepted parameter trait (#15976).

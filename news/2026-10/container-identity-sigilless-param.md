# `=:=` sees a raw parameter's Scalar and a dynamic method result as a value

`val =:= try val."{val.^name}"()` (DB::Xoos's identifier-or-placeholder test) answered True for
a `\val` parameter bound to a `$` variable, so every bound value was emitted as a quoted
identifier instead of a `?` placeholder. A sigilless name now owns a container only when the
binder aliased a caller `$` variable (the same facts `.VAR` reflects), and a `try` over a
run-time-named method call counts as a bare value for container identity, as in Rakudo.
DB::Xoos `t/01-sql.t` passes 18/18.

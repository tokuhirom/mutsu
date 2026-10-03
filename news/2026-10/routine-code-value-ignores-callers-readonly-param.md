# A top-level sub called through a code value no longer inherits its caller's readonly param

`my $s = &setter; $s($v)` from a routine with its own readonly `$v` parameter
died with "Cannot assign to a readonly variable or a value" when `setter`
assigned the outer `my $v` (#11070). The readonly registry is keyed by bare
name and follows the dynamic call stack; a nested routine's code value already
recorded its written free variables' state where `&name` was mentioned (#10389),
but a top-level routine recorded nothing, because the mentioning frame says
nothing about variables that belong to no running routine frame.

A `FunctionDef` now carries the readonly snapshot of the frame it registered in
(`capture_declaring_readonly_state`, as methods do since #11054), and
`sub_value_for_routine` hands it to a top-level routine's code value. The two
reconcile functions became one: `ReadonlySnapshot` records whether it was taken
at a declaration, and such a snapshot only clears a parameter/loop-alias mark it
lacks -- a top-level sub is hoisted, so its snapshot can predate a later
`my $x := 42`, whose immutable mark must survive.

Known gap: when the caller's parameter shares its name with an outer variable
bound to an immutable value, the caller's alias mark has replaced the
immutable one in the registry, so the write is allowed instead of refused.

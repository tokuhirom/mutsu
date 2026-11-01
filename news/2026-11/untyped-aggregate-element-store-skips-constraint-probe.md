# An element store into an untyped `my @a` / `my %h` no longer probes for a type record

`DeclaredConstraint::Untyped` (ADR-13757) let a store into a scalar the code
declares without a constraint skip the by-name type-record probe, but
an `@`/`%` slot never took it: its declaration was only recorded as
"declared here", so every element store into an untyped array or hash read
the running frame's record (`decl_type_constraint`, four times per
`@a[$i] = $i`, ~1.7k instructions of ~22k) to learn there is none.

An `@`/`%` declared without a constraint now records `Untyped`. Two things keep
a slot out of it: a typed declaration or parameter of the same slot (a new
`DeclaredConstraint::Declared`, folded to `Unrecorded` on the slot, makes the two
conflict whichever is recorded first, where `Unrecorded` used to give way to the
next declaration), and a `:=` that binds a typed container to the name, which
writes its constraint by name: a declaration that is itself a bind, and a slot in
`rebound_slots` (final only after `compute_free_vars`, which folds in a nested
closure's `:=`; applied in `compute_fresh_locals`). Pinned by
`t/collections/untyped-aggregate-slot-element-store.t`. Part of #14702.

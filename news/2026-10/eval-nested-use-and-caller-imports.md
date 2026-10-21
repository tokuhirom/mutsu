# EVAL sees a routine imported by a nested `use` and by its caller's `use`

`EVAL q[{ use M; plain-fn }]` failed with "Undeclared name" when `plain-fn` was
an ordinary export of `M`, and so did `use M; EVAL q[plain-fn]`: the name check
knew imported routines only through a source scan of the module, and not at
all when the `use` was the caller's (whose routines live in a lexical import
set, not the routine registry).

A `use` nested in a block of the EVAL'd unit now has a copy pre-run ahead of
the CHECK-time validation, like a top-level one (a hook module's moves, see
`eval-nested-hook-use-prerun`), so the source scan is skipped for it; and the
name check asks the lexical import sets of the scopes the EVAL runs in
(`lexically_imported_routine`), as the routine check already did (#13441).

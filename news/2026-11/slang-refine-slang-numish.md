# Slangs by execution: `$*LANG.refine_slang` and the core `numish` action override

`$*LANG.refine_slang('MAIN', $grammar-role, $actions-role)` registers a slang from the roles
themselves, as `overload::constant` does (#15804), and an actions role may override the core
`numish` action: every numeric literal after the `use` goes through it, `callsame` answers the
core literal node, and the node the action attaches replaces it (ADR-15762). Role instances
(`Numish[...]`, a parenthesised type name) and anonymous `role { }` arguments are read by their
declared name. A module that calls `refine_slang` is now recognised as slang-activating at parse
time. Still open for overload::constant itself: composing a parametric role *instance* through
`^add_role` (#17007), `my`/anonymous roles in slang registration (#17008), and running the user's
handler closures at parse time (#16888).

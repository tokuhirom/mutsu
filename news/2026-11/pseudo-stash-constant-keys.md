# `CALLER::<foo>` and `DYNAMIC::<foo>` find a sigil-less constant

A sigil-less key of the frame-walking pseudo-stashes names a term, and a
sigil-less `constant` is stored under its term key (#9962), which the lookup
never tried. So `constant foo = 3; sub g { DYNAMIC::<foo> }` answered `Nil`
instead of 3, and `CALLER::<foo>` answered `Nil` where rakudo raises
`X::Caller::NotDynamic`. `CALLER::`, `CALLERS::` and `DYNAMIC::` now look a
sigil-less key up as a term first and then as itself (a lexical type), and
`X::Caller::NotDynamic` names such a key as spelled (`'C'`, not `'$C'`). Part
of #17846.

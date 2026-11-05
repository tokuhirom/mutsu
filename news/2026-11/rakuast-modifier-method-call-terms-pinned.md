# RakuAST: the invocant and name of a method-call condition keep their ranges

`$n = $n.abs if $n.WHAT` (an assignment target, a method-call source and a method-call condition
whose invocants are equal `$n` terms) answers the ranges `raku` 2026.09 does for the condition: the
call 26-33, its invocant 26-28 and its name 29-33. The case was open as #16656 (the invocant and
name had no range); it is fixed on `main` and is now pinned in
`t/rakuast/rakuast-modifier-equal-terms-origins.t`.

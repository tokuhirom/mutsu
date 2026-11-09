# `$x ~~ S/.../.../` yields the substituted copy

`$x ~~ S/a/b/` and `$x ~~ TR/a/b/` evaluated the copy and then smartmatched it against `$x`,
answering `False` (or `True` when nothing changed). Like `s///` and `tr///`, the non-destructive
forms are now the value of the smartmatch (`!~~` still compares), so a routine ending in
`$selector ~~ S/^ '<' (.*) '>' $/$0/` returns the stripped string. Found through
Qwiratry's `Selector.normalize-name`.

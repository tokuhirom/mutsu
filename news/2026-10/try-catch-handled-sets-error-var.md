# A handled exception is left in `$!`

`try { CATCH { default { } }; die "x" }; say $!.message` printed an error because mutsu restored
`$!` to its pre-`try` value whenever a CATCH handled the exception. Rakudo leaves the handled
exception in `$!`. The inline handler runner now publishes it (a `.resume` still restores the
prior value, as before). Closes #12904.

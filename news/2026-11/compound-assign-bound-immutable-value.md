# Modifying a name bound to an immutable value names the value

`my $x := "a"; $x .= uc` (and `$x += 1`, `$x ~= "b"`) died with the generic
"Cannot assign to an immutable value". rakudo reports a read-modify-write of
such a name by its value, "Cannot modify an immutable Str (a)", and keeps the
generic wording for a plain `$x = 1`. The compiler now emits a separate
read-only check, `CheckReadOnlyModify`, for the target of a compound
assignment and of `.=`. It refuses exactly what the plain check refuses and
words an immutable binding's refusal by its value, as an X::Assignment::RO
carrying `.value` (#18986).

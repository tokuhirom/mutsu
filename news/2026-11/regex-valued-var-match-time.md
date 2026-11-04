# A `Regex`-valued `$var` of a pattern is read at match time

A `Regex` value interpolated as `$re` used to be spliced into the pattern text. It is now a
match-time `VarInterp` entered as a capture-isolated group (in its defining scope when it
closed over one), as the `<$re>` form is, so a pattern that names a `Regex` variable has one
body for every value. The atom of a `$<a>=$re` alias keeps the splice (#14510).

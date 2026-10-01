# `@a[^2]=@o` no longer parses as a reduction meta-assign

With no spaces around `=`, the statement parser read `@a[^2]=@o` as the infix reduce-assign
`@a [^2]= @o` and failed with "Unsupported reduction operator". A `^N` / `^$n` / `^@x` / `^(..)`
bracket body is now recognised as an upto-range subscript, so it is parsed as a slice followed by
plain assignment. `^^`, `^ff` and `^fff` remain reduction operators. Closes #10453.

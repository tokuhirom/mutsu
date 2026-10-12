# `sprintf("%d", "x")` throws `X::Str::Sprintf::Directives::BadType`

A numeric directive (`%d %i %u %x %o %b %e %f %g %c`) given a `Str` that does
not numify now throws `X::Str::Sprintf::Directives::BadType` (`.directive` is
the base letter, `.value` the string) instead of formatting a silent `0`. A
numeric `Str` (`"42"`, `"4.5"`, `""`) still formats as before.

# `$Pkg::var` interpolates in a regex

A package-qualified scalar such as `$P::v`, `$OUR::v` or `$GLOBAL::v` in a regex
used to fail with "Unrecognized regex metacharacter :". The regex tree reader, the
interpolation pass and the parse-time validator now read the `::` as part of the
variable name (#14510).

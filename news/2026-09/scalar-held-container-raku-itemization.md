# Scalar-held arrays and hashes retain their `.raku` itemization

`$scalar = @array` and `$scalar = %hash` now retain Rakudo's `$` marker in
`.raku`, including when the value is an `is Array` or `is Hash` subclass. The
shared container remains usable for method calls, indexing, and write-through
mutation.

Fixes #7658.

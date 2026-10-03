# Parse a trailing package separator on scalar names

Scalar references such as `$pkg::.^name` and `$pkg::.WHO` now parse and
evaluate. Rakudo treats the trailing separator as part of the scalar reference
syntax; the reference still reads `$pkg` in these forms.

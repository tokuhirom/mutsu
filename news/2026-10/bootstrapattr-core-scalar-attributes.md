`Int.^attributes`, `Num.^attributes`, and `Str.^attributes` now report their
`$!value` attribute as a `BOOTSTRAPATTR`, matching Rakudo's type identity and
type checks. Other built-in attributes continue to report as `Attribute`.

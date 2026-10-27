# Calling a subset type object coerces through its base type

`subset Short of Int where * < 100; Short("50")` and `my $t = Short; $t("50")` now
answer `50`: the argument is coerced with the base type's protocol (`"50".Int`) and
then checked against the subset; a value outside the subset is `X::Coerce::Impossible`.
Before, the named call died with "Unknown function: Short" and the variable call with
"No such method 'CALL-ME'". Found through XML::Class's `t/090-types.t` (the
subset-typed attribute is deserialised with `$type($text)`).

# regex `$( $re )` matches a Regex value as a pattern

A `$( code )` interpolation inside a regex whose code returns a `Regex` value used to match the
text of the regex source literally, so `"a1" ~~ / a $( $r ) /` with `$r = rx/(\d)/` failed.
The scalar form now treats a `Regex` result as a pattern, as `<$r>`, the list form and Rakudo do.

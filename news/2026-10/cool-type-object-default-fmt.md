# Str.fmt and Int.fmt on a type object answer the empty string

`Cool.fmt` is `sprintf($format, self)`, so `Str.fmt` and `Int.fmt` (the default `%s` format) on a
`Cool` type object stringify it to `""` with the usual uninitialized-value warning, instead of
being `X::Method::NotFound` (#12849). A call with a format already formatted the empty string.

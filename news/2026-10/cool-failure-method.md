# Cool.Failure

`"message".Failure` (Raku 2022.06, `method Failure(Cool:D:)`) was missing, so
`String::Utils`' `shorten` died with `X::Method::NotFound` instead of returning a
`Failure`. It is now a `Cool` row of the method table that wraps the receiver in an
`X::AdHoc` `Failure`; a type object declines.

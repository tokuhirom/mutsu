# `X::IO::Resolve` message and `Whatever:D` in no-match profiles match rakudo

`"foo/../bar".IO.resolve(:completely)` now fails with `Failed to completely resolve
IO::Path.new("foo/../bar", ...)` (the exception's `.path` is the `IO::Path` itself) instead of
`X::IO::Resolve: foo/../bar`, and a `Whatever`/`HyperWhatever` argument in a
`Cannot resolve caller ...` profile is spelled `Whatever:D` / `HyperWhatever:D` rather than
`Any:D` (#16879, found by the raku-doc differential sweep).

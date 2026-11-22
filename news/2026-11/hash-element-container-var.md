# A hash element holding a caller's container reflects as `Scalar`

A `*%h` slurpy element bound from a named argument written as a variable
(`f(:a($y))`) holds the caller's container: writing `%h<a>` writes `$y`.
Its `.VAR`, though, read through that container and answered the value's
type (`Int`) instead of `Scalar`, and `.VAR.of` died.

A `.VAR` subscript (and a `:=` bind source) of a hash element now keeps the
container the element holds, as an array element and a stash entry already
did, so `%h<a>.VAR.^name` is `Scalar` (#18372).

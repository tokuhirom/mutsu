# `%?RESOURCES` entries are `Distribution::Resource`, not `IO::Path`

A `%?RESOURCES` value was an `IO::Path` instance carrying a hidden `resource`
marker. It is now an instance of the class `Distribution::Resource`, which
shares the `IO::Path` method rows through its dispatch shape but is not an
`IO::Path` (`.^name`, `.WHAT` and `~~ IO::Path` match Rakudo); `.IO` answers a
real `IO::Path`. The type-matching special case for the marker is gone (#15818).

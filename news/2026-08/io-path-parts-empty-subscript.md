# IO path parts support empty positional subscripts

`IO::Path::Parts` now returns its `volume`, `dirname`, and `basename` values
for `$parts[]`, matching its documented `Positional` behavior.

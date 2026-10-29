# `gather { .. }.iterator` is not `is-lazy`

A class whose `iterator` returns a gather's iterator now answers `.grep`,
`.map` and the other `Any` list methods with a Seq that is not lazy, as in
Rakudo, so `.Array.Bag` on the result works. The built `Iterator` marked
itself lazy because it pulls from a gather on demand; it now asks the source
list's `is-lazy`. Regression of Grammar::Extractor since #15288 (#15425).

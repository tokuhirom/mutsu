# A `does Sequence` instance keeps its list protocol under a `but` mixin

`C.new but Role`, where `C does Sequence` supplies `iterator`, lost the
iterator-driven `Str`/`list`/`elems`/... answers: prefix `~`, interpolation and
`.list` saw an opaque object (`C<19>`, a one-element list). The mixin now
delegates to the wrapped instance's `Sequence` protocol. This is what Red's
`ResultSeq` (a `Sequence` mixed with `ProjectedModel`) needs for `is $seq, (...)`.

Inside the class's own `iterator` `self` stays the mixed value, so a mixin's
`method of` wins there too (Red's `ProjectedModel` relies on it to build the
projected row type).

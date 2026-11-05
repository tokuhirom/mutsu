# `open(%?RESOURCES<x>)` opens the resource file

The routine form `open($resource)` stringified a `Distribution::Resource` and
tried to open a file named after the object (#16155). `open` now resolves its
path argument through the same helper as `slurp`, so `IO::Path` values keep
their captured cwd and resources open their real file. Pinned by
`t/modules/compunit/open-distribution-resource.t`.

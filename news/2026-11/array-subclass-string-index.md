# `@r["2"]` on an `is Array` subclass numifies the index

A `[ ]` subscript on an instance of an `is Array` subclass now numifies a string
index before calling `AT-POS`, as rakudo does, instead of dying with
"does not support associative indexing" (#16006).

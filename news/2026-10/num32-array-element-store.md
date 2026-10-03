# num32 array element stores round to single precision

A direct element store, `push`, `unshift` and `append` on a `my num32 @n` array kept double
precision, while the initial assignment already rounded. The element-write paths only coerced
native integer element types; they now also coerce `num32`, matching rakudo (#11450).

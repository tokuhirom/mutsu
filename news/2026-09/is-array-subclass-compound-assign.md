# Compound assignment on an `is Array` subclass element reads the old value

`@f[1] += 5` (and `-=`, `~=`, `//=`) on a variable whose container is an `is Array`
subclass read the current element through the associative `AT-KEY` path, so it saw `Any`
and stored `Any + 5`. The compound-assignment desugar now reads with a true positional
subscript when the subscript is applied directly to an `@`-variable; nested intermediates
(`@a[0][1] += 1`) keep the lenient associative read. Pinned by
`t/collections/is-array-subclass-compound-assign.t`.

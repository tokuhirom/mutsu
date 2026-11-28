# Inline map callbacks return output statements' values

An inline `.map` callback ending in statement-form `say`, `put`, or `print`
now returns that statement's `True` value. The same tail value is used as the
predicate by inline `.grep` and `.first`, including when the current element
itself is false. The callback compiler keeps its tail on the VM stack, where
all three loops already read their results, instead of storing it in `$_`.

Fixes #19355.

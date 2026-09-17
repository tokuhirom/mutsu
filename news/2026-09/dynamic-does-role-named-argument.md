`does` now stores the value from a named role initializer such as
`Role(:attribute(value))`, rather than retaining the named `Pair` as the role
attribute value. This fixes dynamic role composition used by parameter traits.

Closes #8577.

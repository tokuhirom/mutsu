# A role's own name inside a pun method denotes the role group

When a role method ran on the role's pun (`Role.method`), the role's own
(qualified) name inside the body resolved to the punned class, so
`Role.^parameterize(T)` returned it unchanged and `Monad::Maybe.some(42)` built
an unparameterized type. The self-reference rule for lexically mangled classes
now skips a package that is only a composition of the role of that name.

A `Nil` passed to a `has T $.value` attribute of a parametric role through
`.new(value => Nil)` is now reset to the attribute type's default (`Any`/`Int`)
on the generic constructor path too, so `Monad::Maybe.some(Nil).gist` is
`Some((Any))`.

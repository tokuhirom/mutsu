# Exported `&trait_mod:<is>`, `Attribute.set_build` arguments, `^role_arguments`

Working the ecosystem distribution `Trait::Env` (10 of its 12 test files pass now, up from 1):

- A multi dispatcher value (`%EXPORT<&trait_mod:<is>> = &trait_mod:<is>`) now selects the candidate whose
  parameters really accept the call; `:%env` no longer takes `True`.
- A `&trait_mod:<is>` installed by `sub EXPORT` counts as a trait handler for attribute and variable
  traits even without another `trait_mod:<is>` in scope.
- `Attribute.set_build` closures are called as `(object, default)`, the second argument being the `is default`
  value or the attribute's type object, and a deferred `.map` result is reified.
- `$var.var = ...` inside a variable trait handler assigns the declared variable.
- `.^role_arguments` works on curried roles such as `Associative[Int]`.

Remaining files: #12278 (variable traits run at run time) and #12279 (stale `%*ENV` in a build closure).

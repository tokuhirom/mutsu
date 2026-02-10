# S14 - Roles and Parametric Types

Reference: `old-design-docs/S14-roles-and-parametric-types.pod`

Covers role composition, parametric roles, and traits.

---

## Role Declaration and Composition

- [x] `role Name { ... }` declaration
- [x] `does Role` in class declaration
- [ ] Conflict detection on composition (same-named methods)
- [ ] Conflict resolution with `multi` or `proto` stubs
- [ ] Attributes in roles (`has` and `$!private`)
- [ ] Method delegation with `handles`
- [ ] `also is` / `also does` inside role body

---

## Compile-time Composition

- [x] Classes incorporate roles with `does`
- [ ] Conflicting methods cause composition failure
- [ ] Resolution via multi or explicit override
- [ ] Diamond composition handling

---

## Run-time Mixins

- [ ] `does` operator for runtime role application
- [ ] `but` operator for runtime mixin
- [ ] Mixin of precomposed role sets
- [ ] Non-associative semantics

---

## Parametric Roles

- [ ] `role Name[Type $T] { ... }` type parameters
- [ ] `role Name[::T] { ... }` type capture parameters
- [ ] Parametric subtyping (R[T1] narrower than R[T2])
- [ ] `of` keyword as sugar for single type parameter
- [ ] `Array of Recipe` = `Array[Recipe]`

---

## Role Meta-objects

- [ ] ParametricRoleGroupHOW
- [ ] ParametricRoleHOW
- [ ] CurriedRoleHOW
- [ ] `our` declarations forbidden in roles

---

## Traits

- [ ] `trait_mod:<is>` verb
- [ ] `trait_mod:<does>` verb
- [ ] `trait_mod:<will>` verb
- [ ] `trait_mod:<of>` verb
- [ ] `trait_mod:<returns>` verb
- [ ] `trait_mod:<handles>` verb
- [ ] Custom trait definitions

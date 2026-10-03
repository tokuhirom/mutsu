# Same-named parametric and plain roles keep their own submethods and types

A parametric role and a plain role may share one name. Definitely declares
`role Some[::Type]` beside `role Some`, but mutsu's by-name role tables keep
one entry per name. Two things leaked between the candidates:

- **Construction submethods.** `Some[Int].new` ran the plain `Some`'s `TWEAK`.
  The role-submethod walk, now in its own file
  `runtime/methods_object_role_submethods.rs`, takes `BUILD`/`TWEAK`/`DESTROY`
  from the candidate of the shape each composition used.
- **Attribute types.** The plain `Some`'s untyped `$.value` was checked
  against the parametric `Some`'s `Type`. A role attribute's recorded type now
  applies only when the candidate being composed (or instantiated directly)
  declares that attribute with a type.

Separately, a `-->` return type written with an imported short name of a
parameterized role (`--> Maybe[Int]` for an exported `Definitely::Maybe`)
now accepts a value that mixes the role in, as the smartmatch already did.

Definitely's `t/02-Bind` now passes 3/3, and `t/01-Definitely` passes 33 of
34. The remaining test is #11634 (a punned role method's `self.Bool`).

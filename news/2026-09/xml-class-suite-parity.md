# XML::Class: seven interpreter gaps behind its test suite

Working the `XML::Class` distribution (5 of 15 test files were red) exposed seven independent
bugs, each fixed generally and pinned under `t/`:

- An `is copy` parameter no longer takes its assignment constraint from a caller variable that
  merely shares its name (`$element = Nil` inside a recursive multi retyped the copy).
- A parametric container type object (`Associative[Str]`, what `Attribute.type` returns for
  `has Str %.h`) now binds to `Cool %o` / `Cool @o` by its element type.
- Multi ranking resolves a candidate's bare role/class name in the package that declared it, so a
  role-typed candidate (`ContentX`) outranks a wider `Attribute` one when the role lives in a
  package-qualified registry name.
- `class B { class B { ... } }` declares `B::B` with its own methods and attributes instead of
  merging them into the enclosing `B`.
- A named parameter's sub-signature (`:$x! (Str:D $name, :$over-ride!)`) now takes part in
  multi applicability, and a bare scalar no longer unpacks as a one-element list.
- An attribute trait named like a parametric role's parameter (`is xml-element` after a class
  did `XML::Class`) stays a named trait rather than dispatching positionally on the leaked
  parameter binding.
- `class Test::Bool` at file scope no longer becomes the `Bool` that the Test module's
  `done-testing --> Bool:D` resolves.

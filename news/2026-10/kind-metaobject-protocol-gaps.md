# Metaobject gaps found with Kind

Running the `Kind` distribution (parametric types built on `Metamodel::SubsetHOW`)
closed several gaps:

* a metamethod (`method ^kind`) is a method of the type's metaclass, so
  `method ^parameterize(|args) { self.kind: |args }` reaches it through `self`;
* `Metamodel::Primitives.parameterize_type` flattens Slip arguments like its
  `*@params` slurpy (`Empty` no longer becomes a parameter named `Empty`);
* `SubsetHOW.new_type` accepts any Callable `:refinement` (an object with its
  own `CALL-ME`), and `set_language_revision` is supported;
* `Metamodel::Archetypes.new: :nominal` builds an archetypes object;
* `.bless` and `.CREATE` die on an `is repr<Uninstantiable>` type, like `.new`.

`Kind`'s `t/02-meta.t` instantiation and `t/01-typecheck.t` class/grammar
subtests now pass; the role subtests (core roles such as `Blob` are modeled as
classes) and the `Perl6::` name prefix of the metaclasses are filed separately.

# `Type.HOW.^mixin: Role` changes the metaobject, so parametric subsets work

`SpeedLimit.HOW.^mixin: MetamodelX::ParametricSubset` (Kind::Subset::Parametric)
did nothing: the HOW-dispatch argument rewrite replaced the metaobject being
mixed into with its metaclass's type object, so the mixin never reached the
cached `.HOW`. Three fixes make the real module pass 8/8 like rakudo:

- `^mixin` on a metaobject instance keeps the instance as the invocant, so the
  composition persists exactly like `HOW does Role`.
- `Metamodel::Primitives.parameterize_type($type, *@params)` now takes its
  parameters as a slurpy list (a single list argument is flattened into it).
- A role's `has &!code` attribute on a `but` mixin is read from the role cell
  rather than the wrapped object when a method is entered on the fast path.

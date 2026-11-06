# A typed container names a package-scoped enum by its declared name

`my RangeEndCases @a` where the enum lives in a package printed `Array[Codes::RangeEndCases]` for `.WHAT`/`.raku`
while rakudo (and mutsu's own `RangeEndCases.^name`) say `Array[RangeEndCases]`: rakudo does not qualify an enum by its
package. Typed `Array`/`Hash` names, the `my E % = ...` header of a typed hash's `.raku`, and `Array[E]`/`Hash[E]`
parameterizations now use the enum's declared name; classes, roles and subsets stay package-qualified (#16913).

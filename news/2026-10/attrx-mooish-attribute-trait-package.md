# AttrX::Mooish: attribute traits see `$*PACKAGE`, `get_value(...) = $v`, mixin role attributes in the MOP

`AttrX::Mooish`'s `trait_mod:<is>(Attribute:D $attr, :$mooish!)` died on its first line, so none of its 35 test
files loaded. Four general gaps behind that:

- An attribute trait handler now sees the package being declared as `$*PACKAGE`, as a method trait's already did
  (for a role: the individual parametric role, with a `ParametricRoleHOW`). This is cluster 54eb219f
  (`Dynamic variable $*PACKAGE not found`: also Async::Workers, Cooklang, OO::Plugin).
- `$attr.get_value($obj) = $v` stores into the object (`Attribute.get_value` returns the attribute's container).
- A role mixed into an object at run time (`$attr does R`, `$o but R`) contributes its attributes to
  `.^attribute_table` / `.^get_attribute_for_usage`, and `Attribute.get_value`/`set_value` read and write the
  mixin's role cell.
- `^add_method` of a method with a role mixed in (`anon method {...} but Tag[$x]`) installs the method instead of
  silently dropping it.

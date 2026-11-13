# An attribute bound to an Array, Hash or value hands out no Scalar

A `$` attribute `:=`-bound to something that is not a Scalar has no Scalar
container in rakudo: after `$!val := @a`, a raw read of `$!val` is the Array
itself (`.VAR.^name` is `Array`), and after `$!val := 42` it is the Int. mutsu
stores such a binding as a cell (the aggregate's own share cell, or a readonly
binding cell for a value), and two paths handed that cell out as if it were a
Scalar:

- an `is raw` method returning the attribute (`method get() is raw { $!val }`,
  the `AttrContainerRef` tail), and
- `Attribute.set_value` given a `$`-variable bound straight to an Array or Hash
  (`my $x := Array.new`, a raw parameter): it wrapped the aggregate in a Scalar
  of its own, which is right only for an aggregate that came *out of* a Scalar
  (an itemized one, `my $x = [1, 2]`).

AttrX::Mooish's lazy attributes bind the built Array or Hash back into the
attribute through exactly these paths, and its "attribute is de-mooified" check
(`$attr.get_value($obj).VAR.^isa(Array)`) failed. Its `t/010-base` (both
variants) now passes and its `t/070-typecheck-coerce.custom` output matches
rakudo's.

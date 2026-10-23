# Red's `^join` alias models: five more interpreter gaps closed

`Bla.^join(Ble, *.id == *.bla-id).^all` queried `SELECT * FROM "ble_1"` in Red 0.2.7 (no
join, no columns) and the rows it did read lost their foreign key. Following it down found
five independent gaps, each fixed and pinned by its own test:

- **`$type.HOW does Role` on a `new_type` type** was dropped: the metaobject `new_type`
  built carried no `__mutsu_how_target`, so the mixin never reached the type's registered
  HOW (Red's `alias.HOW does RAlias[...]` gives the alias its `columns`, `tables`, `join-on`).
- **`self.Metamodel::ClassHOW::compose(type)` on a HOW with a role mixed in** fell through
  to the user's own `compose` again instead of the native one, recursing until the stack ran
  out.
- **`$.attr` of an attribute `:=`-bound to a type object**, read on a `but`-mixin invocant,
  handed back the binding cell, so `$.rs-class.new` and `$.rc.^name` saw a cell, not the
  class. The itemized read now looks through the cell.
- **`Attribute.package`** returned the string form of the type object it was built with
  (`(Foo)`, which reads as a coercion type), so `Red::Column.model` was a `CoercionHOW` and
  `.^as` on it died. It now answers the type object itself.
- **A `Pair` whose key is a `gather` Seq** keyed a hash by the empty string: the key was read
  before the deferred source had run (`self.find-column-name => self.find-value` in
  `Red::AST::Eq.should-set`, which is how a relationship's child rows get their foreign
  key). The key is pulled once when the Pair is made.

Refs #14094.

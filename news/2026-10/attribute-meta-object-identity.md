# An attribute has one `Attribute` meta-object

`class Foo { has $.foo }; Foo.^attributes[0] === Foo.^attributes[0]` was
`False`, and the two lookups had different `.WHICH` values (#10004). Rakudo
keeps exactly one meta-object per attribute. mutsu built a fresh instance on
every `.^attributes` call, so anything keyed by identity saw two attributes: a
`SetHash` of attributes, a `===` check, or a `.WHICH`-keyed cache in a module.

mutsu still builds the introspection object on every lookup. Its keys come
from the current class registration, so they are never stale. What changed is
the instance id: every build of the same attribute now gets the same one, and
that id is what `===` and `.WHICH` compare. The id is keyed by owner, sigil
and name. This also covers a redeclared class: since nothing is cached, the
id stays stable and no invalidation is needed. A role attribute composed into
two classes is two attributes, an inherited attribute is its parent class's
one, and both match Rakudo. The `$=pod` declarant of a documented `has` uses
the same identity, so `$=pod[0].WHEREFORE === Foo.^attributes[0]`. Built-in
type attributes (`Rat.^attributes`) and `Attribute`'s own bootstrap attributes
are stable too.

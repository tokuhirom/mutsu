# Assigning to a non-rw accessor names the attribute's value

`$obj.attr = v` on a public attribute that is not `is rw` raised
`X::Assignment::RO` with the text `method 'attr' is not rw`. Rakudo's accessor
hands back the attribute's value, so the refusal reads `Cannot modify an
immutable Str (is baz)` (`... an immutable 'Any' type object` while the attribute
holds none), and `.typename`/`.value` are filled in. The accessor path now raises
that exception (#14266, AttrX::Mooish's `t/013-validations`).

# A core type name in a signature is not captured by a loaded package type

`unit module AttrX::Mooish` does `use AttrX::Mooish::Attribute` and declares
`trait_mod:<is>(Attribute:D $attr, :$mooish!)`. The type qualification pass (#14051) turned the bare
`Attribute` into the loaded `AttrX::Mooish::Attribute` role, so `has $.x is mooish` died with "Can't
use unknown trait 'is' -> 'mooish'" and every AttrX::Mooish test file failed to start. Rakudo finds a
bare type name lexically, then in GLOBAL, and only then in the package's own stash, so a core type
(`Attribute`, `Path`, ...) wins; the parser pass now leaves core type names alone.

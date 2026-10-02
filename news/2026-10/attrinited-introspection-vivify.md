# Introspection reads vivify attributes for `nqp::attrinited`

`nqp::attrinited` answered `0` for an attribute that `.raku`, `.gist`, `dd`,
`eqv` or `Attribute.get_value` had just read. Rakudo answers `1` there, because
each of those reads the attribute the way `nqp::getattr` does, and that read
vivifies the slot (#11003).

The default `.raku` and `.gist` rendering now reads each public attribute
through `AttrMap::get_vivify`, and so does `Attribute.get_value` for its own
attribute. `eqv` between two instances of the same class with the default
`.raku` vivifies both operands' public attributes. As in Rakudo, `.Str`, a
private attribute, `eqv` against an object of another type, and `eqv` of an
object with itself read nothing.

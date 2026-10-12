# Role-body constants stay per role, mixins expose their attributes, exported `Endian` wins

Three gaps found while running Native::Packing's own suite:

- A `my constant` in a role body is now persisted as a static of the composing type, so two roles
  using the same constant name no longer read each other's value. A parameterized candidate that
  composes the plain candidate of its own group (`role R[Int $e] does R`) now runs the plain
  candidate's body too, so its lexicals and constants exist.
- `Attribute.get_value` / `set_value` look through a `$obj but Role` mixin to the wrapped object.
- An exported enum with a qualified name (`my enum Pkg::Endian is export`) replaces the built-in
  `Endian` marker in the importer, so `Endian::Vax` resolves to the imported enum.

Native::Packing: t/basic, host-packing, inheritance, read and sub-records now match rakudo.

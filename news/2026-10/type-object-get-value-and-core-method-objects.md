# `Attribute.get_value` on a type object dies; core-type method objects called on an instance

Found running Data::Dump's suite:

- `$attr.get_value(H)` on a type object now raises "Cannot look up attributes in a H type object"
  instead of answering `Nil`, so `try { ... } // 'undefined'` takes its fallback.
- A method object of a core type with no builtin catalog row (`$m.^can('pos')[0]`, `.from`,
  `.orig` of `Match`) called with an instance of that type dispatches the method on it, in both
  the VM and `call_sub_value` paths.

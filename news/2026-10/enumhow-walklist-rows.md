# EnumHOW's metamethods and WalkList's methods are rows

`Metamodel::EnumHOW`'s own metamethods are now rows of the method table: `enum_values`, `elems`, `enum_from_value`,
`enum_value_list`, `add_enum_value`, `set_export_callback`, `compose_values`, `is_composed` and `compose`. So are the
`invoke`, `reverse` and `quiet` of the `WalkList` that `.WALK` returns (ADR-11276 §9.140). Each metamethod now has
its own routine, replacing one by-name match per type. The answers are unchanged.

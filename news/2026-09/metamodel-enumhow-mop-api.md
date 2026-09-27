# `Metamodel::EnumHOW` builds enums through the MOP

An enum can now be built the way `raku-doc`'s `Type/Metamodel/EnumHOW.rakudoc`
describes, without the `enum` declarator:

```raku
my constant E = Metamodel::EnumHOW.new_type(:name<E>, :base_type(Int));
E.^add_role(NumericEnumeration);
E.^compose;
E.^add_enum_value("Warning" => 0);
E.^add_enum_value("Failure" => 1);
E.^compose_values;
say E.^enum_values;        # {Failure => 1, Warning => 0}
say E.^enum_from_value(1); # Failure => 1
```

`Metamodel::EnumHOW.new_type` now gives the minted type a value list that
`.^add_enum_value` appends to. `.^set_export_callback` sets a callback,
`.^compose_values` runs it once, and `.^is_composed` reports `.^compose`. As in
Rakudo, the value objects are kept exactly as they were passed in, so the doc's
`Pair`s come back from `.^enum_from_value` unchanged. `.^enum_values`,
`.^elems`, `.^enum_from_value` and `.^enum_value_list` now read from one value
list for both kinds of enum, so a declared enum and a MOP-built one give the
same kind of answer.

The `NumericEnumeration` and `StringyEnumeration` roles now exist. Before this,
`NumericEnumeration.^name` answered `Str`. Every declared enum with numeric
values does `NumericEnumeration`, and every one with string values does
`StringyEnumeration`, both for its values and for its type object
(`enum Col <R G>; R ~~ NumericEnumeration` is `True`).

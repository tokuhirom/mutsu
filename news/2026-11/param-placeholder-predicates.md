# One definition of a parameter placeholder name

The names a parameter without a written variable executes under (`__subsig__`, `__type_only__`,
`__literal__`, `__type_capture__T`) were compared as string literals in a dozen compiler, parser,
`light_closure_shape` and runtime sites, each with its own subset. They are now recognised by
`ast::is_placeholder_name` / `ParamDef::is_placeholder`, and the spellings are constants
(`LITERAL_PARAM` added). ADR-15761 records the decision for the first family of #15761.

# One probe for instances that carry a built-in's state

An instance of a user subclass of `Proc::Async`, `Supplier` or
`IterationBuffer` keeps the built-in's state in a reserved attribute. Three
per-family owner probes and the extra `IterationBuffer` site are replaced by
`method_table::carried_state_owners_of`, which reads the instance once, and
the interpreter's single owner lookup (`carried_state_owners_of`) that adds the
opaque shapes and a stream-decoder subclass's MRO answer. A method the
subclass declares itself wins over the row for every family.

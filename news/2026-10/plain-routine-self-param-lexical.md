# A plain routine's `$self` parameter is its own lexical

`sub ($self) { $self ~ self.^name }` inside a method read the parameter for both the
`$self` variable and the `self` term, because plain routines bound the parameter under
the `self` env key. Plain routines now bind and read it under the reserved `$self`
lexical key exactly as methods do: the compiler no longer maps `$self` to `self`, the
binder (general, legacy and positional slot map) targets the reserved key, the light
named-call path leaves such signatures to the general binder, and the frame no longer
installs the argument as the invocant (#15464).

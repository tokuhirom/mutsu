# Math::Fitting now passes under mutsu

`Math::Fitting` 0.0.5's four test files now pass under mutsu, moving the
distribution from partial parity (2/4 files and 12/25 assertions) to green
parity (4/4 files and 25/25 assertions).

The fixes cover three interpreter paths used by the distribution and its
`AttrX::Lazy` dependency: sigil-prefixed custom operators no longer shadow
adjacent array variables, methods installed through `^add_method` retain their
lexical captures, and entries from `.^method_table` and
`.^private_method_table` invoke the compiled method frame with the supplied
invocant. Reflective `.^lookup` calls continue to use their direct callable
payload, including grammar tokens and the NQP cursor protocol.

Pinned by `t/lang/operators/prefix-operator-overloading.t` and
`t/oo/method/private-method-table.t`.

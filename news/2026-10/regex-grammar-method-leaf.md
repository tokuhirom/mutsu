# A grammar method called from a rule runs on the compiled regex engine

A `<name>` call that names a plain grammar method instead of a rule (`<.panic>`,
`<.expect('x')>`) used to leave the compiled regex engine. It was bridged through the tree
walk's producer. The compiled engine now calls the method itself, once, on the calling rule's
cursor, and takes the one end the method answers. This removes the `grammar-method` bridge
(56 uses over `t/grammar`, `t/regex` and `t/modules`) and the method half of `args-method`.
The remaining half is renamed `args-unbound`. This is ADR-0135 §8, Slice E, part eighteen.

The method call now lives in its own module, which both engines share. Under the
`MUTSU_RX_DIFF` differential mode, the call is recorded and replayed like a code block. Before
this change, the walk's comparison run called the method a second time, together with its side
effects.

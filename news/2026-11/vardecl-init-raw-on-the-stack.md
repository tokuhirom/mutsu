# A custom container trait's raw initializer travels on the stack

`my @a is Tied = 'x'` hands `Tied.STORE` the initializer as written, which the
declaration's own store would otherwise coerce into an Array. The raw value
used to wait for the trait in `Interpreter::vardecl_init_raw`, set by
`StashVarDeclInit` and taken by whichever `ApplyVarTrait` ran first.

Now `StashVarDeclInit { keep_raw }` leaves the raw value on the stack below the
value the store pops, and the compiler names its one consumer:
`ApplyVarTrait { raw_init: true }`, the declaration's first type-named trait.
The store's own question ("will a trait see the raw value?") is the new
`TRAIT_INIT` bit of the mark-context word, consumed by that store and isolated
across calls like the rest of the family. ADR-10779 D3: `Interpreter` loses a
field (85 → 84).

The value can no longer go astray: an expression-position declaration whose
container trait takes an argument (and so is not applied there) used to leave a
stash behind for the next declaration's trait; an `is default` applied first
used to take the value from the container trait after it; and a `STORE` that
itself declared a traited container shared the one field with its caller.

# A Seq bound to an `@` parameter is cached

Binding a `Seq` (`@x.reverse`, `@x.map(...)`) to an `@` parameter, positional or named,
shared the Seq's consumption state with the callee, so the first consuming method
(`.sort`, `.grep`) stole it and the second died with `X::Seq::Consumed`. Rakudo binds `@`
parameters through `.cache`; mutsu now marks the Seq cache-requested at bind time.
Found via App::Moneymoor (`t/15-invariants-property.rakutest`); pinned by
`t/routines/signature/seq-bound-to-array-param-rereadable.t`.

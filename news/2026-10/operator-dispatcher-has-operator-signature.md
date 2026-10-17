# A block-imported operator's value has the operator's signature

Inside a block whose `use` imports user candidates of a built-in operator,
`&infix:<cmp>` is a dispatcher over those candidates that falls back to the
built-in (ADR-8751 stage 2). Its signature was the bare capture a generated
proto has, so `&infix:<cmp>.arity` was `0` and `.count` was `Inf`, and
`.sort(&infix:<cmp>)` took the value for a one-argument key mapper. The value
now carries the operator's positional signature, the one mutsu's own
`&infix:<cmp>` reports, so it introspects and sorts like the operator (#13792).

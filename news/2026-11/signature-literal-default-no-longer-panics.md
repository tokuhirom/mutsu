# A signature literal's hash/array default no longer panics a debug build

`:(%a = {:a(2)}).params[0].default` compiled the default straight from the parser's expression
entry, which still carried `Expr::Spelled` wrappers (ADR-12199); the compiler's `Spelled` arm is a
`debug_assert!`, so `roast/S06-signature/introspection.t` died with `exit 101` on every debug build
(`scripts/dev gate`, the CI debug TAP job). `synthesized_thunk` now strips the spellings first, as
the regex `"..."` thunk already did.

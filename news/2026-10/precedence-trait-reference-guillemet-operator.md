# `is tighter(&infix:«|>»)` now resolves guillemet operator references

Shell::DSL declares `sub infix:«|>»(*@o) is assoc<list>` and `sub infix:«|>>»($a, $b) is assoc<non>
is tighter(&infix:«|>»)`. Three gaps kept `a |> b |>> c` from reading as `a |> (b |>> c)`: the
RakuAST round trip refused the `&infix:«|>»` reference because the symbol contains a `>`, a
reference to a declared operator with no precedence trait of its own had no level, and a module's
scan lost that level once its parse scopes were gone. The reference is now canonicalised to
`infix:<|>`-style text, a trait-less declared operator defaults to the additive level, and exported
`sub EXPORT` entries carry that level to importers.

# `×` and prefix `+` no longer numify an opaque object to 0

An imported `multi infix:<×>` whose `where` clauses decline falls back to the core `×`. The
fallback received its operands still wrapped as the caller's variables, so an object was never
asked for `.Numeric` and silently became 0 (Rakudo: `Cannot resolve caller Numeric(...)`).
`coerce_infix_operand_numeric` now reads through the wrapper. Prefix `+` swallowed a failing
`.Numeric` on a user object the same way and answered 0; it now propagates the error. `Math::Vector`'s
`t/01-basics.rakutest` (`dies-ok { $v1 × $v7 }`) passes, 201/201 as on Rakudo.

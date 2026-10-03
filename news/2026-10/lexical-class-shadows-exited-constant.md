# A `my class` outranks a same-named constant from an exited block

`{ constant RIS = Int }` followed by `{ my class RIS { ... }; RIS.hi }` resolved
the second block's bare `RIS` to `Int`: the constant is `our`-scoped, and the
bareword fallback that reaches such a constant after its block has exited
(`term_binding`'s package-store probe) ran ahead of the `env` binding the
lexical class declaration had just made. That fallback now yields when `env`
binds the name to the type object a declaration of that spelling installed
(including a lexical class's ADR-0047 storage name), so the innermost
declaration wins; outside the class's block, and next to an unrelated
same-named `$`-scalar, the bare name still reaches the constant. Found through
Rake's `t/01-basic.rakutest` (#11261).

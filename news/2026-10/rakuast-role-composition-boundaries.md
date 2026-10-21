# RakuAST preserves role composition and caller bindings

Role headers retain the written order of parent clauses, `is export`, `is rw`
and custom traits. The parser records that order before separating the fields
used by compilation; conversion no longer refuses a mixed header merely
because its order was lost. Anonymous source roles retain their existing
declaration name as private model provenance instead of consuming another
global name during lowering. The public constructor rendering omits it.

Absolute parent types retain their leading empty name part, including the
core-parent shadowing case. Parameterized variable role traits can carry
plain string and numeric arguments as expression nodes, alongside the
existing parsed argument path and qualified type arguments.

Lowered subs, methods and closures run the existing `does` parameter analysis.
Readonly parameters can therefore write a composed value back through the
scalar or element passed by the caller, using the existing compiled binding
path. ADR-14144's documented value-copy divergence is unchanged. Parentheses
also retain the distinction between an ordinary role coercion and the direct
call spelling of a mixin initializer.
Colonpair value delimiters retain the caller's container identity for named
`is rw` and `is raw` arguments.

`RakuAST::Role.new` and `RakuAST::RoleBody.new` support constructed declarations,
including parameterization, body and mixed trait lists. The focused contract
is `t/rakuast/rakuast-role-composition-boundaries.t`, measured against Rakudo's
RakuAST frontend. Existing role, module and writeback regressions exercise the
same paths. The older body-composition test now uses Rakudo's valid
`also does Role` syntax.

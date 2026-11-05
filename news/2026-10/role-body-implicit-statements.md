# A role body carries rakudo's implicit statements, so `.DEPARSE` keeps its methods

`Q|role R { method r { 1 } }|.AST.DEPARSE` printed `role R { }`: rakudo's deparser reads a role body as
`statements.skip.head(*-1)` ("lose fabricated return value"), and mutsu's role body held only the written
statements, so the method was skipped. A role body now holds the statements rakudo's tree adds around its
own (a `Role::ResolveInstantiations` first, `nqp::list(MetaObject, nqp::curlexpad)` last), whether it comes
from the parser or from `RoleBody.new`. `.raku` and the lowering leave them out, as rakudo does. Pinned by
`t/rakuast/rakuast-role-deparse-method-body.t`.

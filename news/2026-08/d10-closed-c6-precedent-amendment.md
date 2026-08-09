# ADR-0019 D10 closed: amended completion criterion, blessed the residual raw-Stmt payload

ADR-0019's D10 box ("delete class/role AST registration walkers") is
closed. After D6/D9 landed (dropping `CompiledClassDeclPlan`/
`CompiledRoleDeclPlan::legacy_body` and switching `run_class_body`/
`walk_role_body` to iterate the compiler-built `body_plan` directly) and a
follow-up PR closed two genuinely avoidable `from_stmt` fallback callers,
what remained was a design-note grep criterion — *zero* `Stmt::`-matching
registration code outside token/rule routing and the `augment class`
walker — that the code does not meet by the letter and that isn't worth
meeting.

The remaining raw-`Stmt` reads (`ClassBodyOp::Other`/`ClassSub`/
`CodeAlias`/`ProtoMethod`/`LeavePhaser`'s own `raw` field,
`RoleBodyOp::Deferred`'s `raw`) are all payload extraction from an
already-classified typed op — never AST-shape dispatch, since the op's
kind is already decided by `body_plan`/`role_body_plan` at compile time.
This is architecturally identical to the ADR's own C6 precedent, already
accepted as permanent: a compiled routine's `FunctionDef` keeps its raw
AST body around for a pure-interpreter fallback and certain structural
facts.

The D10 completion criterion is amended accordingly: **no AST-shape
dispatch in the class/role registration path**, outside token/rule
routing and the `stmt_pool`-fed augment walker. A typed op may carry its
raw statement as an opaque payload for one-shot field extraction once its
kind is already known — that is not dispatch. Under that reading, D10 is
satisfied and closed; the six named payload reads are the exhaustive,
enumerated, permanent exceptions, and any *new* raw-`Stmt` match added to
the registration path outside this list would be a regression against the
box.

Two of the six reads are cheap boolean decisions rather than payload
extraction and could be precomputed at compile time to slightly harden the
invariant further — filed as
`todo/tickets/adr0019-d10-precompute-stub-and-swallow-flags.md` as
optional, low-priority opportunistic follow-up, not a new ADR box.

D2, D3, D4, and D5 remain open in Phase D — D10 closing does not close
Phase D as a whole. See the ADR's own entries for each box's remaining scope.


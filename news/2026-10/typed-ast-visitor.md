# AST analyses use a typed visitor instead of serializing the AST to JSON

Four compile- and registration-time analyses — the frame-lexical inner-sub proof (ADR-0113),
the "does this statement mention a role parameter?" check for parameterized role bodies, and TRIR
inlining's "does this body mention `return`?" — used to answer their question by serializing the
subtree with `serde_json` and walking or grepping the JSON. That cost a full copy of the tree per
question and confused identifiers with string literals: `say "EVAL"` disqualified an inner sub
from the frame-lexical fast path, and `my constant Y is export = 'K'` in `role R[::K]` was
deferred to composition, so it could not be imported until something composed the role.

`src/ast_visit/` now provides one read-only, exhaustive AST visitor (ADR-0137). Every identifier
position is reported with a typed `NameKind`, literal data is never reported, and a new AST variant
or field is a compile error in the walker. All four analyses are ported onto it, and no
`serde_json` serialization of `Stmt`/`Expr` is left in the compiler, runtime or TRIR (#10441).

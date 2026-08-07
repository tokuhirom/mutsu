# ADR-0019 D1: class declarations precompute stub and trusts as plan data

D1 asked to put package open/reopen, parent edges, repr, visibility,
lexical/package aliases, and source-order metadata into `CompiledClassDeclPlan`
operations. Most of that was already typed-plan-driven: `exec_register_class_op`
already reads `parents`, `repr`, `is_hidden`/`is_lexical`, and `hidden_parents`/
`does_parents` straight off the plan (from Phase A3/A4), and package
qualification, lexical-site mangling, and EXPORTHOW dispatch already operate on
those typed fields rather than walking `Stmt::ClassDecl`.

The two remaining structural reads against `legacy_body` were both simple
body scans, and both scanned the *same* body twice in different shapes:
`check_class_role_redeclaration` and `class_body_is_stub` independently
re-derived "is this a yada-stub class body" (`class Foo { ... }`), and
`publish_class_shell` walked the body's top level for `Stmt::TrustsDecl` on
every registration. Both move to the compiler: `CompiledClassDeclPlan` gains
`is_stub: bool` (reusing the same `is_stub_routine_body` free function
`CompiledRoutineMetadata` already uses for subs) and `trusts: Vec<Symbol>`
(collected once at plan lowering), threaded through `ClassDeclModifiers` to
`register_class_decl`. `class_body_is_stub` is deleted outright —
`check_class_role_redeclaration` now takes the precomputed bool directly, and
`publish_class_shell` takes `&[Symbol]` instead of the body.

The two callers that build a `ClassDeclModifiers` without a compiled plan
(role-pun registration in `registration_class_augment.rs` and the runtime
mixin-class synthesis in `types/role_mixin_class.rs`) both already pass an
empty body, so they simply supply `is_stub: false, trusts: &[]` — no body to
scan either way.

Pinned by a new compiler unit test
(`class_declarations_precompute_stub_and_trusts`) plus the existing structural
test surface (`t/class-*.t`, `t/lexical-class-identity.t`,
`t/exporthow-class-aspects.t`, the native-repr tests, and the whitelisted
`roast/S12-class/*.t`, `roast/6.c/S12-class/mro-6c.t`). Full `t/`
(27,761 tests) and the roast whitelist pass unchanged.

# NameScope is an explicit parameter of the RakuAST frontend

The frontend no longer reads the names it classifies against from an installed thread-local.
`NameScope` (`src/scope.rs`) is now passed as `names: &NameScope` through `convert`, `lower`,
`finalize_unit` and every helper that classifies a bareword, a trait name or an operator, as
ADR-15760 slice 0 asks. The unit's own declared names and the stack of block scopes moved into
`NameScope` behind `RefCell`, so one shared reference serves the whole walk; the `CURRENT` and
`SCOPES` thread-locals of `bareword.rs` and `with_scope`/`install_scope` are gone.

The entry points that are not a plain parse (string `EVAL`, `Str.AST`, `EVAL($ast)`) build the
scope from the calling lexical scope and hand it in. Native method handlers that construct nodes
outside any such entry use an empty scope, which is what the thread-local held there before.

There is no behaviour change; slice 1 (source-order classification, #15791) is the next step.

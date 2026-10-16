# ADR-8751: A `use` binds its routines lexically in the importing block

- **Status**: Accepted (2026-10-11, by tokuhirom). Supersedes
  [ADR-0108](0108-closure-must-pin-its-defining-blocks-routine-imports.md).
  Stage 0 (the stopgap below) shipped in PR #12838 and is retired by stage 1,
  whose slice 1 (the binding, for plain routines) is done; see §4.
- **Date**: 2026-10-11
- **Addresses**: [GitHub issue #8751](https://github.com/tokuhirom/mutsu/issues/8751)
- **Related**: [ADR-12529](12529-frames-resolve-names-through-the-static-link.md)
  (phase 2 "imports and `my sub` scopes" is this decision's implementation;
  §7 maps #10419 and #9170 to the same phase),
  [ADR-0081](0081-compunit-scoped-module-import-aliases.md),
  [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)

## 1. Context

A closure built inside a block that `use`s a module outlives that block's
dynamic import scope (`pop_import_scope`), and then cannot reach the imported
routine:

```raku
# Inner.rakumod: unit module Inner; sub greet(:$from) is export { ... }
# Outer.rakumod
sub EXPORT(--> Map()) {
    use Inner;
    '&greet' => -> |c { greet(|c, :from<wrapped>) },   # same name as the import
}
```

`raku` prints the wrapped greeting. mutsu died with `Unknown function` (a
different export key), or recursed until the stack overflowed (the key equals
the imported name: the bare call finds the importer's own `&greet`, the closure
itself). The second shape blocks the Deps 0.0.1 distribution.

Rakudo's semantics: `use X` is compile-time and installs X's symbols in the
**lexical pad of the enclosing block**. A call resolves `&greet` by walking the
lexical scopes the code was written in, so the closure keeps the import, the
import wins over anything the importing unit declares at an outer scope, and
the name is invisible after the block (`roast/S11-modules/lexical.t`). Declaring
`my sub greet` in the same block as `use X` is "Redeclaration of symbol" in
rakudo: an import *is* a pad declaration.

## 2. Decision

**An imported routine is a lexical binding of the block that contains the
`use`, resolved through the lexical (static) chain like any other lexical
`&name`.** It is not an alias in a push/pop registry, and a closure does not
carry a private copy of one. This is the "imports" step of ADR-12529 phase 2;
the registry-snapshot machinery (`ImportScope`, `pop_import_scope`,
`imported_env_aliases`) retires as that step lands.

Consequences that pin the design:

1. **Resolution order.** The innermost lexical binding wins, an import
   included, before the importing unit's `GLOBAL` symbols. This removes the
   same-name recursion by construction.
2. **Visibility is the block's lexical extent**, not its dynamic extent. A
   closure, a named `sub` and a `my sub` nested in the block see the import,
   including ones created *before* the `use` statement in that block (the pad
   entry exists from compile time). A callee that is written outside the block
   does not see it, even when called from inside.
3. **Multis and operators bind as dispatcher values** (rakudo: an imported
   `proto f` fully hides an outer `multi f`). They follow ADR-12529 phase 2's
   dispatcher-value step and are out of scope until it lands.
4. **Export names are known only at run time** in mutsu (`UseModule` loads the
   module; custom `sub EXPORT` hooks compute names). The binding is therefore
   *declared* by the compiler (the block has a `use`) and *filled* by
   `UseModule` when the block is entered or the `use` runs, into the block's
   frame binding set, not into a name-keyed global.

## 3. Rejected

- **ADR-0108 §2.1/2.2, a per-closure capture of live aliases consulted as a
  last resort.** Rejected on evidence (Opus review of 2026-10-11, verified in
  code): it is consulted only before "Unknown call", but the same-name case
  never gets that far, because `dispatch_func_call_inner` finds the importer's
  `&name` first (`imported_env_aliases` arm,
  `src/vm/vm_call_func_ops.rs`). It also needs a second capture field and an
  active-closure stack, and it covers neither named subs nor closures created
  before the `use`.
- **ADR-0108 §2.2.1, writing `&name` into the env at `use` time.** Rejected as
  an implementation, kept as a direction: today the closure's captured tier sits
  *below* the caller chain (`vm_closure_dispatch.rs`), so the importer's
  `&name` still wins; `env_callable_is_lexical_override` discards a callable
  whose name equals the call name; and the env is dynamic, so the import leaks
  into callees (`sub other { g() }; { use M; other() }`, which already leaks
  today). The binding must come from the static link (ADR-12529 phase 3), or an
  authoritative override of the captured tier, with provenance rather than a
  name comparison.
- **Searching loaded modules' packages as a fallback.** Dynamic scoping;
  `lexical.t` forbids it.

## 4. Staging

- **Stage 0, shipped (PR #12838).** The compiler records the modules a
  non-top-level `use` names on the enclosing `CompiledCode` and on nested
  closures (`lexical_use_modules`); a bareword call there resolves the module's
  `EXPORT::DEFAULT` routine before the importer's `&name`. A stopgap: it probes
  the registry by module name and calls through `vm_call_function_fallback`
  (marked `TODO`). It fixes the issue's repro and the Deps recursion. It does
  not fix callee leakage, closures created before the `use`, `:tags`/custom
  `EXPORT` hooks, multis or operators, and it is deleted by the next stage.
- **Stage 1 (ADR-12529 phase 2, imports step).** The binding described in §2,
  with `UseModule` filling the block's binding set. Exit: stage 0's
  `lexical_use_modules`, `imported_env_aliases` and the `ImportScope`
  registry snapshot are absent, #10419 and #9170's import remainder close, and
  a `use` inside a routine body no longer bumps `fn_resolve_gen` on every call.

  **Slice 1 done** (`fix/8751-lexical-imports`): the binding half, for
  plain routines. A module `use` in a block (an inline block, a loop body, a
  routine or closure body) names a *lexical import set*, whose id is its
  import list (module, tags, arguments), so a recompile of the block finds
  the same set and every `use` of the same list shares it.
  - The compiler opens the sets of a block's `use`s for the whole block
    (`enter_lexical_use_block`, from `with_import_scope_region` and the
    routine/closure body compilers), so a call site written *before* the
    `use` sees them too; nested routines and closures inherit the chain
    (`Compiler::lexical_import_chain`). The unit's own top level is no block
    region: a top-level import stays the whole unit's, on the registry.
  - The `UseModule` of such a `use` carries the set id
    (`lexical_set_idx`). Its first run collects what the import installed
    (`record_imported_routine_alias`, and the `&name` env keys a `sub EXPORT`
    writes) and binds each plain routine into `ModuleState::lexical_import_sets`:
    a hook's code object as it is, an `is export` routine as its
    `FunctionDef`, which is then called as the registry alias was
    (`compile_and_call_qualified_def`). A code object built at import time
    instead captured the importing frame's view of the module's file-scope
    lexicals per routine, so one routine's rebind of such a lexical
    (`$snaps := ...`) was lost to its siblings.
  - A bareword call under a non-empty chain gets an `OpCode::LexImportCall`
    prefix ahead of its `CallFunc` (named arguments stay in-band). On a hit
    it calls the bound routine through the code-variable call path
    (`CodeVarBinding::Bound` / `Routine`) and skips the `CallFunc`, ahead of every cache
    and of the importing unit's own `&name`; on a miss it falls through.
  - Stage 0's `lexical_use_modules` registry probe is deleted.

  Fixed: a closure, a named sub or a nested closure made in the block keeps
  the import after it exited and when the importing unit has a routine of
  the same name; one made before the `use` sees it; the Deps same-name
  wrapper. Pinned by `t/modules/import-export/use-in-block-binds-lexically.t`.
  Left for stage 1: the import still installs registry aliases for the
  block's dynamic extent, so a callee written outside the block sees it
  (acceptance 4, `todo` in that test), and `ImportScope`/
  `imported_env_aliases` stay until those aliases go; a `use` in a method
  body opens no set yet; a multi family keeps the registry path (stage 2).

  **Slice 2 done** (`fix/8751-imports-skip-registry-alias`): a plain routine
  a block-scoped `use` imports is bound in the lexical import set *instead
  of* being aliased into the registry (`import_module` hands it to
  `bind_routine_lexically` when the running `use` fills a set for the
  importing compunit), so a callee written outside the block no longer sees
  it (acceptance 4). Every read the compiler can place gets the
  `LexImportCall` prefix: a bareword call, `&g(...)` (`CallOnCodeVar`) and a
  `&g` value (`GetCodeVar`). A read by a run-time name falls back to the sets
  of the blocks whose import scope is open (`ImportScopeSnapshot::lexical_sets`):
  `::('&g')` / `&::("g")` (`resolve_code_var`) and an `EVAL` in the block,
  whose compiler is seeded with them. That fallback is dynamic, as the
  aliases were; it only serves names nothing lexical answers. Left for
  stage 1: the import scope still rolls back the registry for what slice 2
  does not bind (multis, protos, operators, classes, variables), so
  `ImportScope` and `imported_env_aliases` stay; a `use` in a method body
  opens no set yet.
- **Stage 2 (phase 2, dispatcher values).** Imported multis, protos and
  operators.

  **Slice 1 done** (`fix/8751-import-multis-operators`, 2026-10-15):
  - **Multis bind as dispatcher values.** A multi family a block-scoped
    `use` imports binds in the set as its dispatcher value
    (`sub_value_from_multi_candidates`), built from the candidates the
    import installed. The importer's same-named family stays out of it:
    rakudo's import binds the module's dispatcher in the block, hiding the
    outer family. When the family's proto only dispatches (body `{*}`,
    signature a bare capture), the import also stops aliasing it into the
    registry (`bind_family_lexically`), so a callee written outside the
    block no longer sees it.
  - **A block's own multi merges.** A block that declares a `multi` or
    `proto` of the imported name merges its candidates with the import
    (`{ use M; multi f(Num) {...} }`, roast `S11-modules/importing.t`
    test 11). The compiler appends those names to the set id
    (`with_block_multis`), and such a family keeps the registry path.
  - **Captured candidates are always in scope.** A dispatcher value's
    captured candidates are ranked without the compunit visibility filter
    (#9944/#11004). They are the value's own, wherever it is called from
    (`choose_best_in_scope_candidate`). The #11004 by-name branch of
    `call_sub_value` takes the declaring unit's family only when it *is* the
    captured set: before, any same-named family there won, so `my &d = do {
    use M; &f }; d('s')` reached the importing unit's own `multi f`.
  - **Binary operators get a prefix.** A binary user operator (`InfixFunc`
    with one right operand and no meta-operator) gets an
    `OpCode::LexImportInfix` prefix, keyed by `infix:<op>`. A closure keeps
    an imported operator after the block exited.
  - **Methods already open sets.** A `use` in a method body opens a set,
    since method bodies compile through the routine body compiler. Slice 1's
    "a `use` in a method body opens no set yet" no longer holds.

  **Slice 2 done** (`fix/8751-import-operators`, 2026-10-15): a user
  `infix:<op>` that is not a built-in operator (`lexically_bindable_operator`)
  binds in the set instead of the registry, so a callee written outside the
  block no longer sees it.

  Every form that uses the operator inside the block reads the set:
  - a binary use and a compound assignment (`LexImportInfix`);
  - `&infix:<op>` (`LexImportCall`);
  - a reduction, triangle reduction or hyper. Their new prefix
    `OpCode::LexImportMeta` runs the `Reduction` with the bound routine as
    its callable (`exec_reduction_op`'s `op_callable`), and the `HyperOp`
    through the `>>[&f]<<` machinery (`exec_hyper_func_op`'s
    `func_override`).

  Two gaps the slice closed, both visible through slice 1's bindings:
  - **Junctions.** A call through a bound `FunctionDef` did not autothread
    a Junction argument. `call_lexical_import_routine` now threads it as a
    by-name call does.
  - **Recompiled bodies.** A `.map` or `gather` body is compiled again from
    its AST, and those recompiles lost the chain. They are now seeded with
    the chain their original compile shared
    (`seed_lexical_import_chain_from`).

  **Slice 3 done** (`fix/8751-import-prefix-postfix-operators`, 2026-10-16):
  a user `prefix`, `postfix` or `circumfix` operator that is not a built-in
  one (`compiled_operator::is_builtin_prefix` / `_postfix` / `_circumfix`)
  binds in the set too. The parser already turns every use of such an
  operator into a bareword call of its full name (`prefix:<¢>(...)`), so
  the existing `LexImportCall` prefix reads the set; only the binding
  decision (`lexically_bindable_operator`) changed. A callee outside the
  block no longer sees the operator. Pinned by
  `t/modules/import-export/use-in-block-binds-affix-operators-lexically.t`.

  Left for stage 2:
  - A user candidate of a built-in operator (`multi infix:<+>`,
    `multi prefix:<->`) and `postcircumfix` keep the registry alias: their
    opcodes find user candidates there. So a callee outside the block still
    sees them.
  - `X`/`Z`/`R` over a user operator do not parse yet (#13591). When they
    do, they need the prefix too.
  - A non-trivial proto keeps its registry alias.
  - Classes and variables keep the `ImportScope` rollback. A top-level
    routine that names a variable by string (`EVAL '$v'`, `::('$v')`) no
    longer sees a block-scoped import of it when the block calls it
    (ADR-12529 phase 3 slice 20, #13604): its frame hides the open block
    imports' variables from the program scope.

## 5. Acceptance

Checked against `raku` and mutsu, under `t/modules/`:

1. The issue's repro and the Deps same-name wrapper (pinned by stage 0 in
   `export-closure-keeps-block-import.t`).
2. A `use` inside an ordinary block with a closure and a named `sub` created
   in it, called after the block (no `sub EXPORT`).
3. A closure created *before* the `use` in the same block sees the import.
4. A callee written outside the block does not see the import when called from
   inside (`sub other { g() }; { use M; other() }` gives the outer `g`).
5. A bare call after the block still dies (`roast/S11-modules/lexical.t`,
   `require.t` test 10).
6. Nested closures inherit the import through ordinary capture.
7. `t/modules/import-export/` and `roast/S11-modules` stay green; stage 1
   carries a callgrind A/B on `bench-fib`, `bench-tak`, `bench-multi-dispatch`
   and FunctionalParsers showing the bareword-call path is not slower, with the
   call site memoizing its `&name` symbol.

## 6. Risks

Taken from the 2026-10-11 review: (1) the captured tier loses to the caller
chain, so stage 1 must not rely on the env tier alone; (2) the same-name
heuristic and the light-call/inline caches run before the override check, so an
imported name must disable them by provenance, not through the process-wide
`export_amp_override_names` set; (3) multis and operators need dispatcher
values first. Prototype the provenance-flagged override against the Deps
shape before committing to the frame-binding layout.

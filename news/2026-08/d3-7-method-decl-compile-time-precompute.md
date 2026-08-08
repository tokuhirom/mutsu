# ADR-0019 D3-7: precompute CompiledMethodDecl at compile time

With D3-1 through D3-6 having unified and de-drifted how the class, role, and
`augment class` walkers each build a `CompiledMethodDecl` from a
`Stmt::MethodDecl`, the natural next step — mirroring D2b's own precedent for
attribute descriptors — was moving that construction from runtime to compile
time.

`CompiledClassDeclPlan` and `CompiledRoleDeclPlan` now carry
`method_decls: Vec<CompiledMethodDecl>`, built once by a new
`compile_method_decls` free function that flattens `SyntheticBlock` the exact
same way `compile_method_name_chunks` (D3-1) already does. Since
`CompiledMethodDecl::from_stmt` is pure AST-to-struct conversion (no compiler
state needed), the two vecs share one position cursor:
`class_body_method_decl`/`role_body_method_decl` now read a clone of the
precomputed descriptor by position instead of calling
`CompiledMethodDecl::from_stmt` on the raw statement at every registration.
Both functions dropped their now-unused `stmt` parameter entirely, since
nothing in either walk reads the raw AST node anymore.

This avoids re-cloning a method's body, param defs, custom traits, and
handles specs on every registration of a class/role declared inside a loop
or a repeatedly-called sub — previously that clone happened fresh each time
`class_body_method_decl` ran, even though the source AST never changes.
`augment_class` (which has no compiled declaration plan at all — `augment
class` still indexes `stmt_pool`) and the role-pun/mixin synthesis paths keep
passing an empty `method_decls` slice, matching `method_name_chunks`'s
existing D3-1 precedent.

## Scoping pass: D3's literal goal is a separate, larger gap

A wider investigation into what D3 ("encode class methods and submethods as
compiled candidates ... without walking `Stmt::MethodDecl`") actually still
requires found a gap much larger than the `CompiledMethodDecl` precompute:
method *bodies* are not compiled by the single main-pass `Compiler` at all,
unlike `SubDecl`, which gets a pool-keyed `CompiledFunction` via
`compiled_routine_keys` (ADR-0019 C1/C3). Instead, a method body is compiled
by a throwaway `Compiler::new()` inside `compile_method_def_in_place_with_dist`
(`accessors_resolve.rs`), triggered from at least nine distinct call sites
(`RegisterClass`/`RegisterRole` VM ops, role mixin composition, three
`augment class` sites, the method-dispatch-cache miss path, `nextsame`
dispatch, BUILD/TWEAK constructor-phase planning, and `class_dispatch.rs`),
memoized only via a `compiled_code.is_some()` guard. `Stmt::ClassDecl`/
`RoleDecl` bodies are never walked by `compile_stmt` the way a `module`/
`package` block is (which recurses and sets `current_package`) — the only
main-pass compile that already touches every method body is
`record_type_body_captures`'s escape analysis, and it throws its result away
and runs under the *enclosing* package rather than the class's own name.

Migrating this fully would mirror Phase C's whole C1-C4 arc, but for methods,
and needs its own multi-slice plan: `effective_param_defs`'s
`::?CLASS`-substitution and auto-`@_`-detection currently run at registration
time reading the real (resolved) class name, which is not always known at the
point the main pass reaches a class body (a computed class name like `class
::($name) {...}` is exactly why D3-1's `method_name_chunks` exists in the
first place); multi-method candidates have no signature-keyed pool slot the
way multi subs do; and a parametric role's method bodies may need
per-composition re-instantiation depending on how type captures reach
compiled bytecode. This is recorded in the ADR as a scoped-but-unstarted
future slice (tentatively D3-8) rather than attempted in one PR.

Verified against the full `t/` suite (27,913 tests) and all 101 whitelisted
roast files under `S12-methods`, `S12-attributes`, `S12-class`,
`S12-construction`, `S14-roles`, and `S06-multi`.
